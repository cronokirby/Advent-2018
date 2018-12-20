{-# LANGUAGE RecordWildCards #-}
import Control.Applicative ((<|>))
import Data.Function (on)
import Data.List (maximumBy, sortOn)
import qualified Data.HashMap.Strict as HM
import Text.ParserCombinators.ReadP


data TimeStamp = TimeStamp
    { year :: Int
    , month :: Int
    , day :: Int
    , minute :: Int
    } deriving (Eq, Ord, Show)

type GuardID = Int

data GuardEvent = GuardBegin GuardID | GuardAsleep | GuardWakesUp deriving (Show)

data Log = Log TimeStamp GuardEvent deriving (Show)

type Input = [Log]

readInput :: String -> Input
readInput = map readLog . lines
  where
    readLog :: String -> Log
    readLog = fst . head . readP_to_S parser
    parser :: ReadP Log
    parser = Log
        <$> parseTimeStamp
        <*> parseEvent
    parseTimeStamp :: ReadP TimeStamp
    parseTimeStamp = do
        _      <- char '['
        year   <- parseInt
        _      <- char '-'
        month  <- parseInt
        _      <- char '-'
        day    <- parseInt
        _      <- char ' '
        _      <- parseInt
        _      <- char ':'
        minute <- parseInt
        _      <- char ']'
        return (TimeStamp {..})
    parseEvent :: ReadP GuardEvent
    parseEvent = parseBegin <|> parseAsleep <|> parseWakesUp
      where
        parseBegin = do
            _  <- string " Guard #"
            id <- parseInt
            return (GuardBegin id)
        parseAsleep = do
            _ <- string " falls asleep"
            return GuardAsleep
        parseWakesUp = do
            _ <- string " wakes up"
            return GuardWakesUp
    parseInt :: ReadP Int
    parseInt = readS_to_P reads


makeGuardMap :: Input -> HM.HashMap GuardID (HM.HashMap Int Int)
makeGuardMap = (\(a, _, _) -> a) . foldr go (HM.empty, 0, 0) . sortOn (\(Log ts _) -> ts)
  where
    go (Log _ (GuardBegin id)) (grid, _, sleepTime) =
        (grid, id, sleepTime)
    go (Log t GuardAsleep) (grid, id, _)            =
        (grid, id, minute t)
    go (Log t GuardWakesUp) (grid, id, sleepTime)   = 
        (insertGrid grid id sleepTime (minute t), id, sleepTime)
    insertGrid grid id startTime endTime = 
        let mins = HM.fromList $ zip [startTime..endTime - 1] (repeat 1)
        in HM.insertWith (HM.unionWith (+)) id mins grid

solve1 :: Input -> Int
solve1 = hash . maximumBy (compare `on` sum . snd) . HM.toList . makeGuardMap
  where
    hash (id, mp) = id * fst (maximumBy (compare `on` snd) (HM.toList mp))

solve2 :: Input -> Int
solve2 = hash . maximumBy (compare `on` maximum . snd) . HM.toList . makeGuardMap
  where
    hash (id, mp) = id * fst (maximumBy (compare `on` snd) (HM.toList mp))
    

main :: IO ()
main = do
    input <- readInput <$> readFile "data/4.txt"
    putStrLn ("Solution 1: " ++ show (solve1 input))
    putStrLn ("Solution 2: " ++ show (solve2 input))
