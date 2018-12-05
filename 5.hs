import Data.Char (ord, toLower)


solve1 :: String -> Int
solve1 = length . foldr go []
  where
    opposite c1 c2 = abs (ord c1 - ord c2) == 32
    go x l@(p:ps)
      | opposite x p = ps
    go x ps = x : ps

solve2 :: String -> Int
solve2 pl = minimum $ map (\c -> solve1 $ filter ((/= c) . toLower) pl) ['a'..'z']


main :: IO ()
main = do
    input <- readFile "5.txt"
    putStrLn ("Solution 1: " ++ show (solve1 input))
    putStrLn ("Solution 2: " ++ show (solve2 input))
