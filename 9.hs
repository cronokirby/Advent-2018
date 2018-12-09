import qualified Data.HashMap.Strict as HM
import Data.List (foldl')


data Circle a = Circle [a] a [a] deriving (Show)

shiftL :: Circle a -> Circle a
shiftL (Circle [] a [])       = Circle [] a []
shiftL (Circle left a (r:rs)) = Circle (a : left) r rs
shiftL (Circle left a [])     = 
    let r:rs = reverse left 
    in Circle [a] r rs

shiftR :: Circle a -> Circle a
shiftR (Circle [] a [])        = Circle [] a []
shiftR (Circle (l:ls) a right) = Circle ls l (a : right)
shiftR (Circle [] a right)     =
    let l:ls = reverse right
    in Circle ls l [a]

shiftN :: Int -> Circle a -> Circle a
shiftN n circle
    | n < 0     = go shiftR (-n) circle
    | otherwise = go shiftL n circle
  where
    go f 0 acc = acc
    go f n acc = go f (n - 1) (f acc)


insert :: a -> Circle a -> Circle a
insert a (Circle left c right) = Circle (c : left) a right

remove :: Circle a -> (a, Circle a)
remove (Circle [] a []) = error "Can't make empty circle"
remove (Circle left a (r:rs)) = (a, Circle left r rs)
remove (Circle (l:ls) a []) = (a, shiftL $ Circle ls l [])


solve1 :: Int -> Int -> Int
solve1 players marbles = maximum . fst $ foldl' go (HM.empty, Circle [] 0 []) [1..marbles]
  where
    addScore k score mp = HM.insertWith (+) k score mp
    playerNum marble = marble `mod` players
    go (scores, circle) marble
      | marble `mod` 23 == 0 =
        let (removed, circle') = remove $ shiftN (-7) circle
        in (addScore (playerNum marble) (removed + marble) scores, circle')
      | otherwise            = (scores, insert marble $ shiftN 1 circle)


main :: IO ()
main = do
    putStrLn ("Solution 1: " ++ show (solve1 452 71250))
    putStrLn ("Solution 2: " ++ show (solve1 452 7125000))
