import qualified Data.HashMap.Strict as HM
import Data.List (maximumBy, minimumBy)
import Text.ParserCombinators.ReadP

type Input = [Rectangle]

readInput :: String -> Input
readInput = map parse . lines
  where
    parse :: String -> Rectangle
    parse = fst . head . readP_to_S parser
    parseInt :: ReadP Int
    parseInt = readS_to_P reads
    parser :: ReadP Rectangle
    parser = do
        char '#'
        id <- parseInt
        skipSpaces
        char '@'
        skipSpaces
        x <- parseInt
        char ','
        y <- parseInt
        char ':'
        skipSpaces
        w <- parseInt
        char 'x'
        h <- parseInt
        return (Rectangle id x y w h)


data Rectangle = Rectangle 
    { getID :: Int
    , getX :: Int
    , getY :: Int
    , getW :: Int
    , getH :: Int
    } deriving Show


points :: Rectangle -> [(Int, Int)]
points (Rectangle _ x y w h) = 
    (,) <$> [x..x + w - 1] <*> [y..y + h - 1]

pointMap :: Input -> HM.HashMap (Int, Int) Int
pointMap = foldr (HM.alter (Just . maybe 1 (+1))) HM.empty . (points =<<)

solve1 :: Input -> Int
solve1 = length . HM.filter (> 1) . pointMap

solve2 :: Input -> Int
solve2 rects = 
    let pMap = pointMap rects
    in getID . head $ filter (all (\p -> HM.lookupDefault 0 p pMap <= 1) . points) rects


main :: IO ()
main = do
    input <- readInput <$> readFile "data/3.txt"
    putStrLn ("Solution 1: " ++ show (solve1 input))
    putStrLn ("Solution 2: " ++ show (solve2 input))