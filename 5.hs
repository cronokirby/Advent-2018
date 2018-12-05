import Control.Monad (guard)
import Data.Char (isUpper, toLower, toUpper)
import Data.Maybe (catMaybes)


data PolymerType = Upper | Lower deriving (Eq, Show)

data Unit = Unit PolymerType Char deriving (Eq, Show)

opposite :: Unit -> Unit -> Bool
opposite (Unit Upper c1) (Unit Lower c2) = c1 == c2
opposite (Unit Lower c1) (Unit Upper c2) = c1 == c2
opposite _ _                             = False

ofType :: Char -> Unit -> Bool
ofType c (Unit _ c1) = c == c1

type Polymer = [Unit]

readPolymer :: String -> Polymer
readPolymer = map convert
  where
    convert c
      | isUpper c = Unit Upper (toLower c)
      | otherwise = Unit Lower c


solve1 :: Polymer -> Int
solve1 ps = 
    length $ foldr go [] ps
  where
    go x []     = [x]
    go x l@(p:ps)
      | opposite x p = ps
      | otherwise    = x : l

solve2 :: Polymer -> Int
solve2 pl = minimum $ map (\c -> solve1 $ filter (not . ofType c) pl) ['a'..'z']


main :: IO ()
main = do
    input <- readPolymer <$> readFile "5.txt"
    putStrLn ("Solution 1: " ++ show (solve1 input))
    putStrLn ("Solution 2: " ++ show (solve2 input))