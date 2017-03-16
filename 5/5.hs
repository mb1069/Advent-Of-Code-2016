-- import Data.Digest.Pure.MD5

has5zeros :: String -> Bool
has5zeros str = Prelude.length (Prelude.filter (=='0') char5) == 5
	where
		char5 = (take 5 str)

getCode :: String -> Int -> String -> String
getCode ind input
	| length ind == 8 = ind
	| has5zeros hash = c ++ getCode (ind+1) input
	| otherwise = getCode (ind+1) input
	where
		hashinput = input ++ show ind
		hash = MD5s (hashinput)
		c = hashinput !! 6

main = do
	let input = "wtnhxymk"
	print $ getCode 0 input