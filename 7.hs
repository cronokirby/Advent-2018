import Data.Char (ord)
import Data.List (partition, sort)


type Input = [(Char, Char)]


readInput :: String -> Input
readInput input = 
    let theWords = map words $ lines input
        takeN n l = head $ l !! n
    in map (\l -> (takeN 1 l, takeN 7 l)) theWords


solve1' :: Input -> String
solve1' input = 
    let alphabet = ['A'..'Z']
        graph = filter (\(a, b) -> a < b) (zip alphabet alphabet) ++ input
    in reverse $ go graph (filter (noIncoming graph) alphabet) []
  where
    noIncoming graph m = null $ filter ((== m ) . snd) graph
    go graph (s:ss) acc =
        let acc' = s : acc
            (xs, ys) = partition ((== s) . fst) graph
            reinsert = filter (noIncoming ys) $ map snd xs
        in go ys (sort (reinsert ++ ss)) acc'
    go [] [] acc = acc


solve2 :: Input -> Int
solve2 input = go (solve1' input) [] 0
  where
    duration c = ord c - 4
    advance workers seconds = 
        let t = minimum (map snd workers)
        in (filter ((> 0) . snd) $ map (\(task, d) -> (task, d - t)) workers, seconds + t)
    canStart a b = null $ filter (== (b, a)) input
    addWorkers n ws [] failed = (ws, reverse failed)
    addWorkers 0 ws ts failed = (ws, reverse failed ++ ts)
    addWorkers n ws (t:ts) failed
      | all (canStart t) (failed ++ (map fst ws)) = addWorkers (n - 1) ((t, duration t) : ws) ts failed
      | otherwise                     = addWorkers n ws ts (t : failed)
    go [] [] seconds = seconds
    go [] workers seconds = uncurry (go []) (advance workers seconds)
    go tasks workers seconds =
        let fetch = 5 - length workers
            (ws, toDo) = addWorkers fetch workers tasks []
        in uncurry (go toDo) (advance ws seconds)


main :: IO ()
main = do
    input <- readInput <$> readFile "7.txt"
    putStrLn ("Solution 1: " ++ show (solve1' input))
    putStrLn ("Solution 2: " ++ show (solve2 input))
