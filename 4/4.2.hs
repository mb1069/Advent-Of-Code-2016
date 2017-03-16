import Data.List.Split
import Data.Char
import Data.List
import Data.Ord

concatList :: [(Char, Int)] -> String
concatList [] = []
concatList (x:xs) = fst x : (concatList xs)

charCount :: String -> [(Char, Int)]
charCount xs = [(a, count a xs) | a<-simplified]
    where
        simplified = rmvDuplicates xs

count :: Char -> String -> Int
count c xs = length (filter (==c) xs)

compareTuples (c1, i1) (c2, i2)
    | i1>i2 = LT
    | i2>i1 = GT
    | i1==i2 && c1>c2 = GT
    | i1==i2 && c2>c1 = LT

rotateString :: String -> Int -> String
rotateString str 0 = str
rotateString str n = rotateString newstr (n-1)
	where
		newstr = map returnNext str

returnNext 'z' = 'a'
returnNext c = chr (ord c + 1)

sortTuples :: [(Char, Int)] -> [(Char, Int)] 
sortTuples xs = sortBy (compareTuples) xs

compare5Chars :: String -> String -> Bool
compare5Chars str1 [] = True
compare5Chars [] _ = False
compare5Chars (s1:xs1) (s2:xs2)
    | s1 == s2 = compare5Chars xs1 xs2
    | otherwise = False

validLetters :: String -> String -> Bool
validLetters str test = compare5Chars (concatList (sortTuples $ charCount str)) test

rmvDuplicates :: Eq a => [a] -> [a]
rmvDuplicates [] = []
rmvDuplicates (x:xs)
    | not (x `elem` d) = (x : d)
    | otherwise = d
    where
        d = rmvDuplicates xs

processRoom :: String -> String
processRoom str
    | validLetters letters orderedLetters = rotatedLetters ++ show roomVal
    | otherwise = rotatedLetters ++ show roomVal
    where
        splt = splitOn "[" str
        test1 = filter (/='-') (splt !! 0)
        letters = filter isLetter test1
        roomVal = read (filter (isDigit) test1) :: Int
        rotatedLetters = rotateString letters roomVal
        orderedLetters = filter (/=']')  (splt !! 1)

main = do
    contents <- readFile "input.txt"
    let lines = Prelude.words contents
    print $ "RESULT:"
    -- print $ sum $ map processRoom lines
    print $ map processRoom lines