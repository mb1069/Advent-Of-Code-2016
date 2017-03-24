import Data.List.Split
import Data.Map (Map)
import qualified Data.Map.Strict as Map

data Bot a = Bot {values :: [Int], low :: Int, high :: Int}
instance Show a => Show (Bot a) where
    show b = show (values b) ++ " " ++ show (low b) ++ " " ++ show (high b)


main = do
    content <- readFile "input.txt"
    let tokens = splitOn "\n" content
    let bot = Bot [1,2] 3 4
    let bots = Map.insert 0 0 (Map.empty)

    print $ Map.toList bots