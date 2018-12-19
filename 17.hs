{-# LANGUAGE TupleSections #-}
import Data.Maybe (fromJust)
import Control.Monad (filterM, forM_, unless, when)
import Control.Monad.Loops (firstM)
import Control.Monad.State (State, execState, gets, modify)
import qualified Data.HashMap.Strict as HM
import Text.ParserCombinators.ReadP (ReadP, readS_to_P, readP_to_S, string, get)


type Input = [([Int], [Int])]

readInput :: String -> Input
readInput = map (fst . head . readP_to_S parser) . lines
  where
    readInt :: ReadP Int
    readInt = readS_to_P reads
    parser :: ReadP ([Int], [Int])
    parser = do
        c  <- get
        _  <- string "="
        x  <- readInt
        _  <- string ", "
        _  <- get
        _  <- string "="
        y1 <- readInt
        _  <- string ".."
        y2 <- readInt
        case c of
            'x' -> return ([x], [y1..y2])
            'y' -> return ([y1..y2], [x])
            _   -> fail "Invalid"


type Pos = (Int, Int)

up :: Pos -> Pos
up (x, y) = (x, y - 1)

down :: Pos -> Pos
down (x, y) = (x, y + 1)

left :: Pos -> Pos
left (x, y) = (x - 1, y)

right :: Pos -> Pos
right (x, y) = (x + 1, y)


data Tile 
    = StillWater
    | Water
    | Clay
    | Free
    deriving (Eq)

isHard :: Tile -> Bool
isHard StillWater = True
isHard Clay       = True
isHard _          = False

isLiquid :: Tile -> Bool
isLiquid StillWater = True
isLiquid Water      = True
isLiquid _          = False


data Grid = Grid 
    { gridMap :: HM.HashMap Pos Tile
    , gridMinY :: Int
    , gridMaxY :: Int
    }

gridFromInput :: Input -> Grid
gridFromInput input = 
    let positions = input >>= (\(xs, ys) -> (,) <$> xs <*> ys)
        ys = map snd positions
        minY = minimum ys
        maxY = maximum ys
    in Grid (foldr (\pos -> HM.insert pos Clay) HM.empty positions) minY maxY


type FlowM a = State Grid a

addTile :: Pos -> Tile -> FlowM ()
addTile pos tile = modify (\(Grid mp minY maxY) -> Grid (HM.insert pos tile mp) minY maxY)

getTile :: Pos -> FlowM Tile
getTile pos = gets (HM.lookupDefault Free pos . gridMap)

isInBounds :: Pos -> FlowM Bool
isInBounds (x, y) = (y <=) <$> gets gridMaxY


flow :: Grid -> Grid
flow = execState doFlow
  where
    doFlow :: FlowM ()
    doFlow = do
        minY <- gets gridMinY
        fall (500, max 1 minY)
    fall :: Pos -> FlowM ()
    fall pos = do
        inBounds <- isInBounds pos
        when inBounds $ do
            addTile pos Water
            let underMe = down pos
            hardBelow <- isHard <$> getTile underMe
            if hardBelow
                then scan pos
                else fall underMe
    hasClay :: Pos -> FlowM Bool
    hasClay pos = fmap (== Clay) (getTile pos)
    stopFlow :: (Pos -> Pos) -> Pos -> FlowM Bool
    stopFlow shift pos = do
        b <- (||) <$> fmap (== Free) (getTile (down pos)) <*> hasClay (shift pos)
        return b
    scan :: Pos -> FlowM ()
    scan (x, y) = do
        let lefts  = map (,y) [x,x-1..]
            rights = map (,y) [x,x+1..]
            findX dir xs = fst . fromJust <$> firstM (stopFlow dir) xs
        minX <- findX left lefts
        maxX <- findX right rights
        let toFill   = map (,y) [minX..maxX]
            leftPos  = (minX, y)
            rightPos = (maxX, y)
        clayLeft  <- hasClay (left leftPos)
        clayRight <- hasClay (right rightPos)
        if clayLeft && clayRight
            then do
                forM_ toFill (`addTile` StillWater)
                scan (up (x, y))
            else do
                forM_ toFill (`addTile` Water)
                unless clayLeft (fall leftPos)
                unless clayRight (fall rightPos)


solve1 :: Grid -> Int
solve1 = length . HM.filter isLiquid . gridMap


main :: IO ()
main = do
    grid <- flow . gridFromInput . readInput <$> readFile "17.txt"
    putStrLn ("Solution 1: " ++ show (solve1 grid))
