import Data.List.Split
import Data.Char
import Data.Tuple as T

getNewDir :: Int -> Char -> Int
getNewDir dir turn
    | turn=='R' = (dir+1) `mod` 4
    | turn=='L' = (dir-1) `mod` 4

-- X Y Dir Dist -> X Y 
getNewPose :: Int -> Int -> Int -> Int -> [Int]
getNewPose x y dir dist 
    | dir == 0 = [x, y+dist]
    | dir == 1 = [x+dist, y]
    | dir == 2 = [x, y-dist]
    | dir == 3 = [x-dist, y]


-- X, Y, Dir, Turn, Dist -> newX, newY, Dist
applyToken :: Int -> Int -> Int -> Char -> Int -> [Int]
applyToken x y dir turn dist = newPose ++ [newDir]
    where
        newDir = getNewDir dir turn
        newPose = getNewPose x y newDir dist

getToken :: String -> (Char, Int)
getToken (dir:n) = (dir, read n :: Int)

applyTokens :: Int -> Int -> Int -> [(Char, Int)] -> (Int, Int)
applyTokens x y dir [] = (x, y)
applyTokens x y dir (t:ts) = applyTokens newX newY newDir (ts)
    where
        tokenDir = fst t
        tokenDist = snd t
        newPose = applyToken x y dir tokenDir tokenDist
        newX = newPose !! 0
        newY = newPose !! 1
        newDir = newPose !! 2

main = do
    contents <- readFile "input.txt"
    let a = splitOn ", " contents
    print $ a
    let a2 = map getToken a
    let xy = applyTokens 0 0 0 a2
    print $ abs (fst xy) + abs (snd xy)