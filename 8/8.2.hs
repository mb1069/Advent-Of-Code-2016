import Data.List.Split
import Data.List


applyRect :: String -> [[String]] -> [[String]]
applyRect str screen = [editLine x | x <- take y screen] ++ drop y screen
    where
        dims = splitOn "x" str
        x = read (dims !! 0) :: Int
        y = read (dims !! 1) :: Int
        editLine l = ["#" | x <- [1..x]] ++ (drop (x) l)


applyColumnRot :: String -> [[String]] -> [[String]]
applyColumnRot str screen = transpose (applyRowRot str (transpose screen))

takeTail :: Int -> [String] -> [String]
takeTail n l = reverse $ take n (reverse l)

applyRowRot :: String -> [[String]] -> [[String]]
applyRowRot str screen = (take y screen) ++ [(rotateLine (screen !! y))] ++ drop (y+1) screen
    where
        params = splitOn " by " str
        y = read (params !! 0) :: Int
        by = read (params !! 1) :: Int
        rotateLine l = take (length (screen !! 0)) (takeTail (by) l ++ l)

applyToken :: String -> [[String]] -> [[String]]
applyToken str screen
    | take 3 str == "rec" = applyRect (drop 5 str) screen
    | take 13 str == "rotate column" = applyColumnRot (drop 16 str) screen
    | take 10 str == "rotate row" = applyRowRot (drop 13 str) screen
    | otherwise = error str

applyTokens :: [String] -> [[String]] -> [[String]]
applyTokens [] screen = screen
applyTokens (x:xs) screen = applyTokens xs newScreen
    where
        newScreen = applyToken x screen

main = do
    contents <- readFile "input.txt"
    let tokens = splitOn "\n" contents
    let row = [" " | x <- [1..50]]
    let scr = [row | x <- [1..6]]
    let screen = applyTokens tokens scr
    print $ concat (screen !! 0)
    print $ concat (screen !! 1)
    print $ concat (screen !! 2)
    print $ concat (screen !! 3)
    print $ concat (screen !! 4)
    print $ concat (screen !! 5)
EFEYKFRFIJ