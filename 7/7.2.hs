import Data.List.Split

abba :: String -> Bool
abba x | length x < 4 = False
abba x = (x!!0)==(x!!3) && (x!!1)==(x!!2) && (x!!0) /= (x!!1)


hasABBA :: String -> Bool
hasABBA xs | length xs < 4 = False
hasABBA (x:xs) = abba (x:xs) || hasABBA xs

getHypSeqs :: String -> Bool -> String
getHypSeqs [] _ = ""
getHypSeqs (x:xs) isHypSeq
    | x == '[' = getHypSeqs xs True
    | x == ']' = " " ++ getHypSeqs xs False
    | isHypSeq = [x] ++ getHypSeqs xs True
    | otherwise = getHypSeqs xs False

getNonHypSeqs :: String -> Bool -> String
getNonHypSeqs [] _ = ""
getNonHypSeqs (x:xs) isNotHypSeq
    | x == ']' = getNonHypSeqs xs True
    | x == '[' = " " ++ getNonHypSeqs xs False
    | isNotHypSeq = [x] ++ getNonHypSeqs xs True
    | otherwise = getNonHypSeqs xs False

evalIPS :: String -> Int
evalIPS xs
    | abbaIn && abbaOut = 0
    | abbaOut && not (abbaIn) = 1
    | otherwise = 0
    where
        abbaIn = hasABBA (getHypSeqs xs False)
        abbaOut = hasABBA (getNonHypSeqs xs True)

main = do
    contents <- readFile "input.txt"
    let tokens = splitOn "\n" contents
    print $ sum $ map evalIPS tokens
