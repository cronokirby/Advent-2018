import Data.List (tails, isPrefixOf)
import qualified Data.Sequence as Seq


digits :: Int -> [Int]
digits i = case divMod i 10 of
    (0, y) -> [y]
    (x, y) -> [x, y]


chocolates :: [Int]
chocolates = 3 : 7 : go 0 1 (Seq.fromList [3, 7])
  where
    go p1 p2 cs = 
        let score1 = Seq.index cs p1
            score2 = Seq.index cs p2
            newDigits = digits (score1 + score2)
            cs' = cs <> Seq.fromList newDigits
            p1' = (p1 + score1 + 1) `mod` length cs'
            p2' = (p2 + score2 + 1) `mod` length cs'
        in newDigits ++ go p1' p2' cs'


solve1 :: Int -> [Int]
solve1 n = take 10 $ drop n chocolates


solve2 :: [Int] -> Int
solve2 xs = length . takeWhile (not . (xs `isPrefixOf`)) $ tails chocolates


main :: IO ()
main = do
    let input = 633601
    putStrLn ("Solution 1: " ++ (show =<< solve1 input))
    putStrLn ("Solution 2: " ++ show (solve2 [6, 3, 3, 6, 0, 1]))
