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

attachTails :: a -> a -> [([a],[a])] -> [([a],[a])]     
attachTails h1 h2 aList = [(xs ++ [h1], ys ++ [h2]) | (xs,ys) <- aList]

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

outputOptAlignments :: String -> String -> IO()
outputOptAlignments string1 string2 = putStrLn $ "There are " ++ lengthList ++  " optimal alignments:" 
    ++ "\n" ++ (unlines $ map alignmentToString list) ++ "There were " ++ lengthList ++ " optimal alignments!"
    where list = optAlignments string1 string2
          lengthList = show $ length list
          alignmentToString = (\(fst,snd) -> "\n" ++ fst ++ "\n" ++ snd ++ "\n")

-- Program optimization
similarityScore' :: String -> String -> Int
similarityScore' xs ys = simLen (length xs) (length ys)
    where 
        simLen i j = simTable!!i!!j
        simTable = [ [ simEntry i j | j<-[0..] ] | i<-[0..] ]

        simEntry :: Int -> Int -> Int
        simEntry 0 0 = 0
        simEntry 0 j = scoreSpace + simEntry 0 (j-1)
        simEntry i 0 = scoreSpace + simEntry (i-1) 0
        simEntry i j = maximum [score x y + simEntry (i-1) (j-1),
                                score x '-' + simEntry (i-1) j,
                                score '-' y + simEntry i (j-1)]
            where
                x = xs!!(i-1)
                y = ys!!(j-1)

        
optAlignments' :: String -> String -> [AlignmentType]
optAlignments' xs ys = optLen (length xs) (length ys)
    where 
        optLen i j = optTable!!i!!j
        optTable = [ [ optEntry i j | j<-[0..]] | i<-[0..] ]

        optEntry :: Int -> Int -> [AlignmentType]
        optEntry 0 0 = [([], [])]
        optEntry 0 j = attachTails '-' (ys!!(j-1)) (optEntry 0 (j-1))
        optEntry i 0 = attachTails (xs!!(i-1)) '-' (optEntry (i-1) 0)
        optEntry i j = attachTails x y (optEntry (i-1) (j-1)) ++
                       attachTails x '-' (optEntry (i-1) j) ++ 
                       attachTails '-' y (optEntry i (j-1))
            where
                x = xs!!(i-1)
                y = ys!!(j-1)

{-
optAlignments'' :: String -> String -> (Int, [AlignmentType])
optAlignments'' xs ys = optLen (length xs) (length ys)
    where
        optLen i j = optTable!!i!!j
        optTable [ [ optEntry i j | j<-[0..]] | i<-[0..] ]

        optEntry :: Int -> Int -> (Int, [AlignmentType])
        optEntry 0 0 = (0, [([], [])])
        optEntry 0 j = map2 ((+) scoreSpace, attachTails '-' (ys!!(j-1))) (optEntry 0 (j-1))
        optEntry i 0 = map2 ((+) scoreSpace, attachTails (xs!!(i-1)) '-') (optEntry (i-1) 0)
        optEntry i j = map2 (maximum, id) ([score x y + (fst $ optEntry (i-1) (j-1)), score x '-' + (fst $ optEntry (i-1) j), score '-' y + (fst $ optEntry i (j-1))], attachTails x y (snd $ optEntry (i-1) (j-1)) ++ attachTails x '-' (snd $ optEntry (i-1) j) ++ attachTails '-' y (snd $ optEntry i (j-1)) )
            where
                x = xs!!(i-1)
                y = ys!!(j-1)
-}

optAlignments'' :: String -> String -> (Int, [AlignmentType])
optAlignments'' xs ys = optLen (length xs) (length ys)
    where
        optLen i j = optTable!!i!!j
        optTable [ [ optEntry i j | j<-[0..]] | i<-[0..] ]

        optEntry :: Int -> Int -> (Int, [AlignmentType])
        optEntry i j = (simEntry i j, genEntry i j)
            where
                simEntry :: Int -> Int -> Int
                simEntry 0 0 = 0
                simEntry 0 j = scoreSpace + simEntry 0 (j-1)
                simEntry i 0 = scoreSpace + simEntry (i-1) 0
                simEntry i j = maximum [score x y + simEntry (i-1) (j-1),
                                        score x '-' + simEntry (i-1) j,
                                        score '-' y + simEntry i (j-1)]

                genEntry :: Int -> Int -> [AlignmentType]
                genEntry 0 0 = [([], [])]
                genEntry 0 j = attachTails '-' (ys!!(j-1)) (genEntry 0 (j-1))
                genEntry i 0 = attachTails (xs!!(i-1)) '-' (genEntry (i-1) 0)
                genEntry i j = attachTails x y (genEntry (i-1) (j-1)) ++
                               attachTails x '-' (genEntry (i-1) j) ++ 
                               attachTails '-' y (genEntry i (j-1))
                
                    where
                        x = xs!!(i-1)
                        y = ys!!(j-1)