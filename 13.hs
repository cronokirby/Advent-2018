{-# LANGUAGE DeriveGeneric #-}
import Data.Function (on)
import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HM
import Data.List (find, sortOn)
import Data.Ord (comparing)
import GHC.Generics (Generic)


data Pos = Pos { posX :: Int, posY :: Int } deriving (Eq, Generic, Show)

instance Ord Pos where
    compare p1 p2 = comparing posY p1 p2 <> comparing posX p1 p2

instance Hashable Pos


data Track = Intersection | Straight | TrackTurnF | TrackTurnB deriving (Show)

newtype Grid = Grid (HM.HashMap Pos Track)

getTrack :: Grid -> Pos -> Track
getTrack (Grid mp) pos = HM.lookupDefault Straight pos mp

readTrack :: Grid -> Pos -> Char -> Grid
readTrack (Grid mp) pos char = Grid $ case char of
    '\\' -> HM.insert pos TrackTurnB mp
    '/'  -> HM.insert pos TrackTurnF mp
    '+'  -> HM.insert pos Intersection mp
    _    -> mp


-- | The direction a cart can be oriented in
data Direction = U | D | L | R deriving (Show)

readDir :: Char -> Maybe Direction
readDir = case c of
    '^' -> Just U
    'v' -> Just D
    '<' -> Just L
    '>' -> Just R
    _   -> Nothing

rotateCC :: Direction -> Direction
rotateCC dir = case dir of
    U -> L
    D -> R
    R -> U
    L -> D

rotateCW :: Direction -> Direction
rotateCW dir = case dir of
    U -> R
    D -> L
    R -> D
    L -> U

turnF :: Direction -> Direction
turnF dir = case dir of
    U -> R
    R -> U
    L -> D
    D -> L

turnB :: Direction -> Direction
turnB dir = case dir of
    U -> L
    L -> U
    D -> R
    R -> D

moveDir :: Direction -> Pos -> Pos
moveDir dir (Pos x y) = Pos (x + dX) (y + dY)
  where
    (dX, dY) = case dir of
        U -> (0, (-1))
        D -> (0, 1)
        L -> ((-1), 0)
        R -> (1, 0)


-- | The directions a cart can decide to move 
data Turn = LeftTurn | StraightTurn | RightTurn deriving (Show)

nextTurn :: Turn -> Turn
nextTurn LeftTurn     = StraightTurn
nextTurn StraightTurn = RightTurn
nextTurn RightTurn    = LeftTurn

turnDir :: Turn -> Direction -> Direction
turnDir t = case t of
    StraightTurn -> id
    LeftTurn     -> rotateCC
    RightTurn    -> rotateCW


data Cart = Cart Pos Direction Turn deriving (Show)

makeCart :: Pos -> Direction -> Cart
makeCart pos dir = Cart pos dir LeftTurn

cartPos :: Cart -> Pos
cartPos (Cart pos _ _) = pos

moveCart :: Grid -> Cart -> Cart
moveCart grid (Cart pos dir turn) = 
    let pos' = moveDir dir pos
        track = getTrack grid pos'
    in case track of
        Straight     -> Cart pos' dir turn
        TrackTurnF   -> Cart pos' (turnF dir) turn
        TrackTurnB   -> Cart pos' (turnB dir) turn
        Intersection -> Cart pos' (turnDir turn dir) (nextTurn turn)


-- | Remove an element from a list, returning true if an element was removed
remove :: (a -> Bool) -> [a] -> ([a], Bool)
remove f = foldr go ([], False)
  where
    go x (acc, found)
      | f x       = (acc, True)
      | otherwise = (x:acc, found)


data GridEvent = Tick | Crash Pos | OneLeft Cart

data GridState = GridState Grid [Cart] [Cart]


readGridState :: String -> GridState
readGridState = foldr go empty . ((\(y, ln) -> zipWith (\x c -> (Pos x y, c)) [0..] ln) =<<) . zip [0..] . lines
  where
    empty = GridState (Grid HM.empty) [] []
    go (pos, c) (GridState grid carts next) = case readDir c of
        Just dir -> GridState grid (makeCart pos dir : carts) next
        Nothing  -> GridState (readTrack grid pos c) carts next

simulate :: GridState -> [GridEvent]
simulate st = let (st', e) = step st in e : simulate st'
  where
    order :: [Cart] -> [Cart]
    order = sortOn cartPos
    step :: GridState -> (GridState, GridEvent)
    step g@(GridState grid [] [only]) = 
        (g, OneLeft only)
    step (GridState grid [] next) = 
        step (GridState grid (order next) [])
    step (GridState grid (cart:carts) next) =
        let cart' = moveCart grid cart
            pos = cartPos cart'
            removeCrashes = remove ((== pos) . cartPos)
            (carts', inCarts) = removeCrashes carts
            (next', inNext)   = removeCrashes next
            found = inCarts || inNext
            event = if found then Crash pos else Tick
            next'' = if found then next' else cart' : next'
        in (GridState grid carts' next'', event)


solve1 :: [GridEvent] -> Pos
solve1 = takeCrash . find isCrash
  where
    -- fine since we're finding on an infinite list anyways
    takeCrash (Just (Crash pos)) = pos
    isCrash (Crash _) = True
    isCrash _         = False

solve2 :: [GridEvent] -> Pos
solve2 = takePos . find isLast
  where
    takePos (Just (OneLeft (Cart pos _ _))) = pos
    isLast (OneLeft _) = True
    isLast _           = False


main :: IO ()
main = do
    gridState <- readGridState <$> readFile "13.txt"
    let events = simulate gridState
    putStrLn ("Solution 1: " ++ show (solve1 events))
    putStrLn ("Solution 2: " ++ show (solve2 events))
