import Data.List.Split
import Data.List


applyRect :: String -> [[Int]] -> [[Int]]
applyRect str screen = [editLine x | x <- take y screen] ++ drop y screen
    where
        dims = splitOn "x" str
        x = read (dims !! 0) :: Int
        y = read (dims !! 1) :: Int
        editLine l = [1 | x <- [1..x]] ++ (drop (x) l)


applyColumnRot :: String -> [[Int]] -> [[Int]]
applyColumnRot str screen = transpose (applyRowRot str (transpose screen))

takeTail :: Int -> [Int] -> [Int]
takeTail n l = reverse $ take n (reverse l)

applyRowRot :: String -> [[Int]] -> [[Int]]
applyRowRot str screen = (take y screen) ++ [(rotateLine (screen !! y))] ++ drop (y+1) screen
    where
        params = splitOn " by " str
        y = read (params !! 0) :: Int
        by = read (params !! 1) :: Int
        rotateLine l = take (length (screen !! 0)) (takeTail (by) l ++ l)

applyToken :: String -> [[Int]] -> [[Int]]
applyToken str screen
    | take 3 str == "rec" = applyRect (drop 5 str) screen
    | take 13 str == "rotate column" = applyColumnRot (drop 16 str) screen
    | take 10 str == "rotate row" = applyRowRot (drop 13 str) screen
    | otherwise = error str

applyTokens :: [String] -> [[Int]] -> [[Int]]
applyTokens [] screen = screen
applyTokens (x:xs) screen = applyTokens xs newScreen
    where
        newScreen = applyToken x screen

main = do
    contents <- readFile "input.txt"
    let tokens = splitOn "\n" contents
    let row = [0 | x <- [1..50]]
    let scr = [row | x <- [1..6]]
    print $ (screen3) !! 0
    print $ (screen3) !! 1
    print $ (screen3) !! 2
    print $ (screen3) !! 3
    print $ (screen3) !! 4
    print $ (screen3) !! 5
    print $ sum (map sum (applyTokens tokens scr))