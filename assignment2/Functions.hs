similarityScore :: String -> String -> Int
similarityScore string1 string2

attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])] 
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

maximaBy :: Ord b => (a -> b) -> [a] -> [a] 
maximaBy valueFcn xs

optAlignments :: String -> String -> [AlignmentType]
optAlignments string1 string2

outputOptAlignments string1 string2

