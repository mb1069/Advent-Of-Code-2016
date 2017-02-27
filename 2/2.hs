import Data.List.Split
import Data.Char
import Data.Tuple as T


getNewPose :: Int -> Int -> Char -> [Int]
getNewPose x y dir
    | dir == 'U' = [x, y+1]
    | dir == 'R' = [x+1, y]
    | dir == 'D' = [x, y-1]
    | dir == 'L' = [x-1, y]
    | otherwise = error "Incorrect dir"

isValidPose :: Int -> Int -> Char -> [Int]
isValidPose x y dir 
    |  xy !! 0 < 2 && xy !! 0 > -2 && xy !! 1 < 2 && xy !! 1 > -2 = xy
    | otherwise = [x, y]
    where
        xy = getNewPose x y dir

getKey :: Int -> Int -> Char
getKey x y 
    | x ==  1 && y == -1 = '9'
    | x ==  1 && y == 0 = '6'
    | x ==  1 && y == 1 = '3'
    | x ==  0 && y == -1 = '8'
    | x ==  0 && y == 0 = '5'
    | x ==  0 && y == 1 = '2'
    | x == -1 && y == -1 = '7'
    | x == -1 && y == 0 = '4'
    | x == -1 && y == 1 = '1'
    | otherwise = error "Invalid key"


applyToken :: Char -> Int -> Int -> [Int]
applyToken h x y = isValidPose x y h

applyLine :: String -> Int -> Int -> (Char, Int, Int)
applyLine [] x y = (getKey x y, x, y)
applyLine (h:xs) x y = applyLine (xs) newX newY
    where
        newPose = applyToken h x y
        newX = newPose !! 0
        newY = newPose !! 1

applyLines :: [String] -> Int -> Int -> String
applyLines [] _ _ = []
applyLines (h:xs) x y = newChar : applyLines xs x2 y2
    where
        newTup = applyLine h x y
        (newChar, x2, y2) = newTup


main = do
    contents <- readFile "input.txt"
    let tokens = splitOn "\n" contents
    let result = applyLines tokens 0 0
    print $ result
