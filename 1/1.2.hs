import Data.List.Split
import Data.Char
import Data.Tuple as T

getNewDir :: Int -> Char -> Int
getNewDir dir turn
    | turn=='R' = (dir+1) `mod` 4
    | turn=='L' = (dir-1) `mod` 4
    | turn=='N' = dir

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

splitTokens :: [(Char, Int)] -> [(Char, Int)]
splitTokens [] = []
splitTokens (x:xs)
    | snd x > 1  =  oneToken : splitTokens (newToken:xs)
    | snd x == 1 = x : splitTokens (xs)
    where
        oneToken = (fst x, 1)
        newVal = snd x
        newToken = ('N', newVal-1)

isInList :: (Int, Int) -> [(Int, Int)] -> Bool
isInList tup [] = False
isInList tup (x:xs)
    | tup == x = True
    | otherwise = isInList tup (xs)

applyTokens :: Int -> Int -> Int -> [(Char, Int)] -> [(Int, Int)] -> (Int, Int)
applyTokens x y dir [] poses = (x, y)
applyTokens x y dir (t:ts) poses
    | isInList newPose2 poses = newPose2
    | otherwise = applyTokens newX newY newDir (ts) newPoses
    where
        tokenDir = fst t
        tokenDist = snd t
        newPose = applyToken x y dir tokenDir tokenDist
        newX = newPose !! 0
        newY = newPose !! 1
        newDir = newPose !! 2
        newPose2 = (newX, newY)
        newPoses = poses ++ [newPose2]

main = do
    contents <- readFile "input.txt"
    let a = splitOn ", " contents
    let tokens = map getToken a
    let sptTokens = splitTokens tokens
    print sptTokens
    let xy = applyTokens 0 0 0 sptTokens []
    print $ abs (fst xy) + abs (snd xy)
    print $ xy