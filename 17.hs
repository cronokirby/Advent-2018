import Control.Applicative ((<|>))
import Data.List (sort)
import qualified Data.HashMap.Strict as HM
import Text.ParserCombinators.ReadP
import Debug.Trace


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


data Tile = Water | HardWater | Clay | Free deriving (Eq, Show)

tileAsChar :: Tile -> Char
tileAsChar Water = '|'
tileAsChar HardWater = '~'
tileAsChar Clay = '#'
tileAsChar Free = '.'

isHard :: Tile -> Bool
isHard Clay      = True
isHard HardWater = True


data Grid = Grid
    { gridMaxY :: Int
    , gridMinY :: Int
    , gridMap :: HM.HashMap (Int, Int) Tile
    }

gridFromInput :: Input -> Grid
gridFromInput input = 
    let positions = input >>= (\(xs, ys) -> (,) <$> xs <*> ys)
        ys = map snd positions
        maxY = maximum ys
        minY = minimum ys
    in Grid maxY minY (foldr (\pos -> HM.insert pos Clay) HM.empty positions)

pprintGrid :: Grid -> String
pprintGrid g@(Grid _ _ mp) =
    let positions = HM.keys mp
        xs = map fst positions
        ys = map snd positions
        minX = minimum xs
        maxX = maximum xs
        minY = minimum ys
        maxY = maximum ys
    in unlines $ map (\y -> map (\x -> tileAsChar $ getTile (x, y) g) [minX..maxX]) [minY..maxY]

getTile :: (Int, Int) -> Grid -> Tile
getTile pos (Grid maxY minY mp) = HM.lookupDefault Free pos mp

addTile :: (Int, Int) -> Tile -> Grid -> (Bool, Grid)
addTile (x, y) tile g@(Grid maxY minY mp)
    | y > maxY  = (False, g)
    | y < minY  = (False, g)
    | otherwise = (True, Grid maxY minY (HM.insert (x, y) tile mp))

addFreeTile :: (Int, Int) -> Tile -> Grid -> (Bool, Grid)
addFreeTile pos tile grid =
    case getTile pos grid of
        Free -> addTile pos tile grid
        _    -> (False, grid)

getWater :: Grid -> [(Int, Int)]
getWater = map fst . HM.toList . HM.filter (== Water) . gridMap

getLiquid :: Grid -> [(Int, Int)]
getLiquid = map fst . HM.toList . HM.filter (\x -> x == Water || x == HardWater) . gridMap


flow :: Grid -> Grid
flow grid =
    let (_, grid') = addTile (500, max 1 (gridMinY grid)) Water grid
    in (go 0 grid')
  where
    go l grid =
        let waters = getWater grid
            w = length waters
            (inserted, grid') = foldr step (False, grid) waters
        in traceShow w $ if getLiquid grid /= getLiquid grid' then go w grid' else grid'
    under (x, y) = (x, y + 1)
    spreadDir grid [] acc     = (False, acc)
    spreadDir grid (p:ps) acc
        | getTile (under p) grid == Free || getTile (under p) grid == Water = (False, p : acc)
        | getTile p grid == Clay         = (True, acc)
        | otherwise                      = spreadDir grid ps (p:acc)
    spread pos@(x, y) grid =
        let leftPos  = map (\x -> (x, y)) [x,x-1..]
            rightPos = map (\x -> (x, y)) [x,x+1..]
            (clayL, lefts)  = spreadDir grid leftPos []
            (clayR, rights) = spreadDir grid rightPos []
            tile = if clayL && clayR then HardWater else Water
        in foldr (\pos (inserted, acc) -> let (i, g) = addTile pos tile acc in (i || inserted, g)) (False, grid)
            (lefts ++ rights)
    step pos (inserted, grid) =
        let down = under pos
            tile = getTile down grid
            (insertedNow, grid') = case tile of
                Water     -> (False, grid)
                HardWater -> spread pos grid
                Clay      -> spread pos grid
                Free      -> addTile down Water grid               
        in (insertedNow || inserted, grid')

            

solve1 = length . getLiquid . flow


main :: IO ()
main = do
    input <- readInput <$> readFile "17.txt"
    let grid = gridFromInput input
    putStrLn ("Solution 1:\n" ++ show (solve1 grid))
