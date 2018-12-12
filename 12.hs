import qualified Data.HashMap.Strict as HM
import Data.List (find)


type Row = String
type Rule = (Row, Char)

readInput :: String -> (Row, [Rule])
readInput input =
    let (x:_:rest) = lines input
        firstRow = words x !! 2
        getRule xs = let ws = words xs in (head ws, head (ws !! 2))
    in (firstRow, map getRule rest)


simulate :: Integer -> Integer -> (Row, [Rule]) -> Integer
simulate gens limit (firstRow, rules) =
    let l = fromIntegral $ length firstRow
        start = 0
        stop = l - 1
        mp = HM.fromList $ zip [start..stop] firstRow
        (a:b:_) = go rules limit mp start stop []
    in a + (a - b) * (gens - limit)
  where
    evolve rules index mp =
        let st = map (\i -> HM.lookupDefault '.' i mp) [index-2..index+2]
        in maybe (st !! 2) snd $ find ((== st) . fst) rules
    go _  0 _ _ _ acc = acc
    go rules n mp start stop acc = 
        let newStart = start - 2
            newStop = stop + 2
            newMp = HM.fromList $ zip [newStart..newStop] ((\i -> evolve rules i mp) <$> [newStart..newStop])
            sm = sum . map fst . filter ((== '#') . snd) $ HM.toList newMp
        in go rules (n - 1) newMp newStart newStop (sm : acc)
           

main :: IO ()
main = do
    input <- readInput <$> readFile "12.txt"
    putStrLn ("Solution 1: " ++ show (simulate 20 20 input))
    putStrLn ("Solution 2: " ++ show (simulate 50000000000 200 input))
