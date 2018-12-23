{-# LANGUAGE RecordWildCards #-}
import Data.Array (Array, Ix, (!), array, assocs, bounds, elems, listArray)
import Data.List (intercalate)
import Debug.Trace
import qualified Data.HashMap.Strict as HM


data YardTile = Open | Trees | Lumber deriving (Eq, Show)

tileFromChar :: Char -> YardTile
tileFromChar '.' = Open
tileFromChar '|' = Trees
tileFromChar '#' = Lumber
tileFromChar _   = error "Invalid Yard Tile"

tileToChar :: YardTile -> Char
tileToChar t = case t of
    Open -> '.'
    Trees -> '|'
    Lumber -> '#'


data Yard = Yard 
    { minX :: Int
    , maxX :: Int
    , minY :: Int
    , maxY :: Int
    , grid :: Array (Int, Int) YardTile
    } deriving (Show)

pprintYard :: Yard -> String
pprintYard (Yard _ x _ _ grd) =
    intercalate "\n" . chunks x . map tileToChar $ elems grd
  where
    chunks n [] = []
    chunks n xs = take n xs : chunks n (drop n xs)


inBounds :: Yard -> (Int, Int) -> Bool
inBounds Yard{..} (x, y) =
    x >= minX && x <= maxX && y >= minY && y <= maxY

adjacent :: Yard -> (Int, Int) -> [YardTile]
adjacent yard (x, y) =
    let ops = map (+) [0, 1, (-1)]
        maybeInside = tail $ (\f g -> (f x, g y)) <$> ops <*> ops
    in map (grid yard !) $ filter (inBounds yard) maybeInside

readYard :: String -> Yard
readYard input = 
    let lns = lines input
        mY = length lns
        mX = length (head lns)
        arr :: Array (Int, Int) YardTile
        arr = listArray ((1, 1), (mX, mY)) $ lns >>= map tileFromChar
    in Yard 1 mX 1 mY arr


mapIx :: Ix i => (i -> a -> a) -> Array i a -> Array i a
mapIx f arr = 
    let newArr = (\(i, a) -> (i, f i a)) <$> assocs arr
    in array (bounds arr) newArr


stepYard :: Yard -> Yard
stepYard y@(Yard mnX mxX mnY mxY grd) =
    Yard mnX mxX mnY mxY (mapIx (step y) grd)
  where
    transition :: YardTile -> YardTile
    transition Open   = Trees
    transition Trees  = Lumber
    transition Lumber = Open
    countNextTo :: Yard -> (Int, Int) -> YardTile -> Int
    countNextTo yard pos target = length $ filter (== target) (adjacent yard pos)
    shouldTransition :: Yard -> (Int, Int) -> YardTile -> Bool
    shouldTransition yard pos Open  = 
        countNextTo yard pos Trees >= 3 
    shouldTransition yard pos Trees =
        countNextTo yard pos Lumber >= 3
    shouldTransition yard pos Lumber = 
        let count = countNextTo yard pos 
        in not $ count Lumber >= 1 && count Trees >= 1
    step :: Yard -> (Int, Int) -> YardTile -> YardTile
    step yard pos tile
        | shouldTransition yard pos tile = transition tile
        | otherwise                      = tile
    
resources :: Yard -> Int
resources Yard {..} = 
    count Trees grid * count Lumber grid
  where
    count tile = length . filter (== tile) . elems


solve1 :: Yard -> Int
solve1 = resources . (!! 10) . iterate stepYard


solve2 :: Yard -> Int -> Int
solve2 yard limit = resources $ go yard 0 HM.empty
  where
    insCounted k v = HM.insertWith (\(newV, _) (_, count) -> (newV, count + 1)) k (v, 1)
    go yard i mp =
        let r = resources yard
        in case HM.lookup r mp of
            Just (lastI, c) | c > 2 ->
                iterate stepYard yard !! ((limit - i) `mod` (i - lastI))
            _ -> go (stepYard yard) (i + 1) (insCounted r i mp)


main :: IO ()
main = do
    yard <- readYard <$> readFile "data/18.txt"
    putStrLn ("Solution 1: " ++ show (solve1 yard))
    putStrLn ("Solution 2: " ++ show (solve2 yard 1000))
