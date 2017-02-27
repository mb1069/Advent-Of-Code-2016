import Data.List.Split
import Data.List
import Data.Char
import Data.Tuple as T


getToken :: String -> [Int]
getToken inp = map read $ words inp

isTriangle :: [Int] -> Int
isTriangle [a,b,c] = if a < b + c && b < a + c && c < b + a then 1 else 0


-- sumValids :: []

main = do
    contents <- readFile "input.txt"
    let tokens = map getToken (lines contents)
    print $ sum (map isTriangle (concatMap transpose $ chunksOf 3 tokens))
    -- let chunks = concatMap transpose $ chunksOf 3 tokens
    -- print $ processTriangles lines
    -- let tups = map getToken lines
    -- let xy = applyTokens 0 0 0 a2
    -- print $ tups