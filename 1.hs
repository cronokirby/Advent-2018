import Control.Arrow ((>>>))
import qualified Data.HashMap.Strict as HM
import Data.List (foldl', scanl)


type Input = [Int]

readInput :: String -> Input
readInput = map readNum . lines
  where
    readNum :: String -> Int
    readNum ('-' : rest) = (-(read rest))
    readNum ('+' : rest) = read rest
    readNum _            = error "Missing + or -"


solve1 :: Input -> Int
solve1 = sum

solve2 :: Input -> Int
solve2 = cycle
    >>> scanl (+) 0
    >>> scanl go (Right HM.empty)
    >>> headLeft
  where
    go :: Either Int (HM.HashMap Int Int) -> Int -> Either Int (HM.HashMap Int Int)
    go acc freq = do
        map <- acc
        let get = HM.lookupDefault 0 freq map + 1
        if get == 2 
            then Left freq 
            else Right (HM.insert freq get map)
    headLeft (Left x : _) = x
    headLeft (_ : xs)     = headLeft xs


main :: IO ()
main = do
    input <- readInput <$> readFile "1.txt"
    putStrLn ("Solution 1: " ++ show (solve1 input))
    putStrLn ("Solution 2: " ++ show (solve2 input))
