import Data.List.Split
import Data.Char
import Data.Tuple as T


getToken :: String -> [Int]
getToken inp = ints
    where
        ints = map read (splilstOn " " inp)

isTriangle :: [Int] -> Int
isTriangle arr
    | ((arr !! 0) + (arr !! 1) > (arr !! 2)) && ((arr !! 0) + (arr !! 2) > (arr !! 1)) && ((arr !! 2) + (arr !! 1) > (arr !! 0)) = 1
    | otherwise = 0

splitList :: [Int] -> [Int] -> [Int]
splitList


processTriangles :: [Int] -> Int -> Int -> Int
processTriangles [] sum = sum
processTriangles (xs) sum i = newSum + processTriangles xs sum i
    where
        sides = (xs !! i, xs !! i+3, xs !! i+6)
        i = i+1 `mod` 4
        newSum = sum + isTriangle sides


-- sumValids :: []

main = do
    contents <- readFile "input.txt"
    let toks = words contents
    let lines = map (read::String->Int) toks
    print $ processTriangles lines 0 0
    -- let tups = map getToken lines
    -- let xy = applyTokens 0 0 0 a2
    -- print $ tups