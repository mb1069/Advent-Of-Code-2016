{-# LANGUAGE OverloadedStrings #-}
import Data.Digest.Pure.MD5
import Data.ByteString.Lazy.Char8 as BS
import Data.List
import Data.Function
import Data.Char

has5zeros :: String -> Bool
has5zeros str = Prelude.length (Prelude.filter (=='0') char5) == 5
	where
		char5 = (Prelude.take 5 str)

getHash :: ByteString -> String
getHash input = show $ md5 input

getCode :: ByteString -> Int -> String
getCode input ind 
	| has5zeros hash = c : nextHash
	| otherwise = nextHash
	where
		hashinput = BS.concat [input, pack $ show ind]
		hash = getHash hashinput
		c = hash !! 5
		nextHash = getCode input (ind+1)

getCodeIndexed :: ByteString -> Int -> [Int] -> [(Int, Char)]
getCodeIndexed input ind positions
	| has5zeros hash && pos<8 && not (pos `Data.List.elem` positions) = (pos, c) : getCodeIndexed input ind (pos:positions)
	| otherwise = nextHash
	where
		hashinput = BS.concat [input, pack $ show ind]
		hash = getHash hashinput
		pos = digitToInt (hash !! 5)
		c = hash !! 6
		nextHash = getCodeIndexed input (ind+1) positions

orderCode :: [(Int, Char)] -> String
orderCode bits = [snd p | p <- ordered bits]
	where
		ordered xs = sortBy (compare `on` fst) xs

main = do
	let input = "wtnhxymk"
	-- print $ Prelude.take 8 (getCode input 0)
	print $ orderCode(Prelude.take 8 (getCodeIndexed input 0 []))
