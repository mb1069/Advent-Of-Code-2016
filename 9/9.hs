import Data.List.Split

dropUntil :: Eq a => (a->Bool) -> [a] -> [a]
dropUntil f [] = []
dropUntil f (x:xs)
    | f x = xs
    | otherwise = dropUntil f xs

decompress :: String -> String
decompress [] = ""
decompress a@(x:xs)
    | x == '(' = decompressedValue ++ decompress (drop dropLength a)
    | otherwise = x : decompress (xs)
    where
        fulltoken = takeWhile (/=')') xs
        token = splitOn "x" (fulltoken)
        numChars = read (token !! 0) :: Int
        nTimes = read (token !! 1) :: Int
        repeatChars = take numChars (dropUntil (==')') xs)
        decompressedValue = concat $ take nTimes (repeat repeatChars)
        dropLength = ((length (fulltoken++repeatChars))) + 2

main = do
    contents <- readFile "input.txt"
    -- print $ length (decompress contents)
    print $ length $ decompress contents