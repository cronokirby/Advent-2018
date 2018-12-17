import Control.Applicative ((<|>))
import qualified Data.HashMap.Strict as HM
import Text.ParserCombinators.ReadP


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


data Tile = Water | HardWater | Clay | Free


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

insertTile :: (Int, Int) -> Tile -> Grid -> Grid
insertTile (x, y) tile g@(Grid maxY minY mp)
    | y > maxY  = g
    | y < minY  = g
    | otherwise = Grid maxY minY (HM.insert (x, y) tile mp)

getTile :: (Int, Int) -> Grid -> Tile
getTile pos (Grid maxY minY mp) = HM.lookupDefault Free pos mp


main :: IO ()
main = do
    input <- readInput <$> readFile "17.txt"
    putStrLn "Solution 1: "