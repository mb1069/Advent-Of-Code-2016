import Data.Function
import Data.List
import Data.Array
import Data.List.Split


count :: Char -> String -> Int
count c [] = 0
count c (s:str)
	| c==s = 1 + count c str
	| otherwise = count c str

mostCommon :: String -> Char
mostCommon str = fst $ head counts
	where
		counts = reverse $ sortBy (compare `on` snd) [(x, count x str) | x <- str]

leastCommon :: String -> Char
leastCommon str = fst $ head counts
	where
		counts = sortBy (compare `on` snd) [(x, count x str) | x <- str]

sumColumns :: [String] -> [String]
sumColumns [] = ["", "", "", "", "", "", "", ""]
sumColumns (x:xs) = zipWith (++) (sumColumns xs) (chunksOf 1 x) 

main = do
	contents <- readFile "input.txt"
	let lines = words contents
	let columns = map mostCommon $ sumColumns lines
	print $ columns
	let leastColumns = map leastCommon $ sumColumns lines
	print $ leastColumns
