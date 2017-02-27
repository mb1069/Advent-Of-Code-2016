import Data.List.Split
import Data.Char
import Data.Tuple as T


getToken :: String -> [Int]
getToken inp = ints
    where
        ints = map read (splitOn " " inp)

isTriangle :: [Int] -> Int
isTriangle arr
    | ((arr !! 0) + (arr !! 1) > (arr !! 2)) && ((arr !! 0) + (arr !! 2) > (arr !! 1)) && ((arr !! 2) + (arr !! 1) > (arr !! 0)) = 1
    | otherwise = 0

processTriangles :: [Int] -> Int -> Int
processTriangles [] sum = sum
processTriangles (xs) sum = newSum + processTriangles (drop 3 xs) sum
    where
        sides = take 3 xs
        newSum = sum + isTriangle sides


-- sumValids :: []

main = do
    contents <- readFile "input.txt"
    let toks = words contents
    let lines = map (read::String->Int) toks
    print $ processTriangles lines 0
    -- let tups = map getToken lines
    -- let xy = applyTokens 0 0 0 a2
    -- print $ tups