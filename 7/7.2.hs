import Data.List.Split

getABAs :: String -> [String]
getABAs xs | length xs < 3 = []
getABAs a@(x:xs)
    | ((a !! 0) == (a !! 2)) && ((a !! 1) /= (a !! 2)) = [aba] ++ getABAs (drop 1 a)
    | otherwise = getABAs (drop 1 a)
    where
        aba = take 3 a

getHypSeqs :: String -> Bool -> String
getHypSeqs [] _ = ""
getHypSeqs (x:xs) isHypSeq
    | x == '[' = getHypSeqs xs True
    | x == ']' = "  " ++ getHypSeqs xs False
    | isHypSeq = [x] ++ getHypSeqs xs True
    | otherwise = getHypSeqs xs False

getNonHypSeqs :: String -> Bool -> String
getNonHypSeqs [] _ = ""
getNonHypSeqs (x:xs) isNotHypSeq
    | x == ']' = getNonHypSeqs xs True
    | x == '[' = "  " ++ getNonHypSeqs xs False
    | isNotHypSeq = [x] ++ getNonHypSeqs xs True
    | otherwise = getNonHypSeqs xs False

invertABA :: String -> String
invertABA str = [(str !! 1)] ++ (take 2 str)

evalIPS :: String -> Bool
evalIPS xs = foldr (||) False [x `elem` abas | x <- abbaOut]
    where
        abas = map invertABA (getABAs (getNonHypSeqs xs True))
        abbaOut = getABAs (getHypSeqs xs False)

main = do
    contents <- readFile "input.txt"
    let tokens = splitOn "\n" contents
    print $ length $ filter (&& True)(map evalIPS tokens)
