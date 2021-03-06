import Data.List.Split
import Data.List
import Data.Map (Map)
import qualified Data.Map.Strict as Map

data Bot = Bot {values :: [Value], low :: Int, high :: Int}
type Index = Int
type Value = Int
type BotMap = Map Int (Bot)

instance Show (Bot) where
    show b = show (values b) ++ " " ++ show (low b) ++ " " ++ show (high b)

-- Retrieve a bot from map or create a new one if ti doesn't exist
getBot :: Index -> BotMap-> Bot
getBot i m
    | (length bots == 1) = bots !! 0
    | otherwise = Bot [] (-1) (-1)
    where
        bots = [b | (i2, b) <- Map.toList m, i2==i]

-- Get indeces of bots with 2 values
botHasTwoValues :: BotMap -> [Index]
botHasTwoValues bots = [i | (i, bot) <- Map.toList bots, (length $ values bot) == 2, low bot > (-1), high bot > (-1)]

-- Give a value to a bot and disperse it as necessary
giveValue :: Index -> Value -> BotMap -> BotMap
giveValue botInd val oldM = process2ValuedBots  newM
    where
        bot = getBot botInd oldM
        vals = sort (val : values bot)
        newM = Map.insert botInd (Bot vals (low bot) (high bot)) oldM

-- Cascade values if a bot has more than one value
process2ValuedBots :: BotMap -> BotMap
process2ValuedBots botmap
    | null twoValuedBots = botmap
    | otherwise = process2ValuedBot (twoValuedBots !! 0) botmap
    where
        twoValuedBots = botHasTwoValues botmap

-- Cascade a particular bot which has 2 values
process2ValuedBot :: Index -> BotMap -> BotMap
process2ValuedBot botInd oldM
	| lowVal > highVal = error ("Invalid ordering")
    | lowVal == 17 &&  highVal == 61 = error ("Found the bot : " ++ (show botInd))
    | otherwise = newM
    where
        bot = getBot botInd oldM
        lowVal = (values bot) !! 0
        highVal = (values bot) !! 1
        changeBot = Map.insert botInd (Bot [] (low bot) (high bot)) oldM
        giveValueLow = giveValue (low bot) lowVal changeBot
        newM = giveValue (high bot) highVal giveValueLow

-- Process a token into a bot's value
applyValueToken :: String -> BotMap -> BotMap
applyValueToken tok map = newMap
    where
        toks = words tok
        botInd = read (toks !! 5) :: Index
        botVal = read (toks !! 1) :: Value
        newMap = giveValue botInd botVal map

-- Process a token into linking bots 
applyEdgeToken :: String -> BotMap -> BotMap
applyEdgeToken tok map = newMap
	where
		toks = words tok
		botInd  = read (toks !! 1) :: Index
		botLow  = read (toks !! 6) :: Index
		botHigh = read (toks !! 11) :: Index
		bot = getBot botInd map 
		newBot = Bot (values bot) botLow botHigh
		newMap = Map.insert botInd newBot map

processToken :: String -> BotMap -> BotMap
processToken tok map
    | (tok !! 0) == 'v' = applyValueToken tok map
    | otherwise = applyEdgeToken tok map

main = do
    content <- readFile "input.txt"
    let tokens = splitOn "\n" content
    let botmap = Map.empty
    print $ foldr (processToken) botmap tokens