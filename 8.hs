import Data.Maybe (catMaybes)
import Text.ParserCombinators.ReadP


data Tree = Tree [Tree] [Int] deriving (Show)


readInput :: String -> Tree
readInput = fst. head . readP_to_S parseInput
  where
    parseInt :: ReadP Int
    parseInt = const <$> readS_to_P reads <*> skipSpaces
    parseInput :: ReadP Tree
    parseInput = do
        childCount <- parseInt
        metaCount  <- parseInt
        children <- count childCount parseInput
        metadata <- count metaCount parseInt
        return (Tree children metadata)


solve1 :: Tree -> Int
solve1 (Tree ts is) = sum is + sum (map solve1 ts)


solve2 :: Tree -> Int
solve2 (Tree [] is) = sum is
solve2 (Tree ts is) = sum . catMaybes $ map (\i -> solve2 <$> ind i ts) is
  where
    ind n []     = Nothing
    ind 1 (x:xs) = Just x
    ind n (x:xs) = ind (n - 1) xs


main :: IO ()
main = do
    tree <- readInput <$> readFile "8.txt"
    putStrLn ("Solution 1: " ++ show (solve1 tree))
    putStrLn ("Solution 2: " ++ show (solve2 tree))
