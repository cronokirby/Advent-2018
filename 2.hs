import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS

type Input = [String]

readInput :: String -> Input
readInput = lines

-- part 1

hasN :: Int -> String -> Bool
hasN n = not . null . HM.filter (== n) . foldr (HM.alter (Just . maybe 1 (+1))) HM.empty

solve1 :: Input -> Int
solve1 input = nLength 3 input * nLength 2 input
  where
    nLength n = length . filter (hasN n)

-- part 2

substrings :: String -> [(Int, String)]
substrings xs = HS.toList . HS.fromList . map (\i -> (i, take i xs ++ drop (i + 1) xs)) $ zipWith const [0..] xs

solve2 :: Input -> Maybe String
solve2 = fmap snd . go HS.empty . (substrings =<<)
  where
    go acc [] = Nothing
    go acc (sub:subs)
      | HS.member sub acc = Just sub
      | otherwise         = go (HS.insert sub acc) subs


main :: IO ()
main = do
    input <- readInput <$> readFile "data/2.txt"
    putStrLn ("Solution 1: " ++ show (solve1 input))
    putStrLn ("Solution 2: " ++ show (solve2 input))
