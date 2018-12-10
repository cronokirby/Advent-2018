import qualified Data.HashMap.Strict as HM
import Text.ParserCombinators.ReadP

data Point = Point (Int, Int) (Int, Int) deriving (Show)

type Input = [Point]

readInput :: String -> Input
readInput = map (fst . head . readP_to_S parser) . lines
  where
    parseInt :: ReadP Int
    parseInt = readS_to_P reads
    parser :: ReadP Point
    parser = do
        _ <- string "position=<"
        x <- parseInt
        _ <- char ','
        y <- parseInt
        _ <- string "> velocity=<"
        vX <- parseInt
        _ <- char ','
        vY <- parseInt
        return (Point (x, y) (vX, vY))


advance :: Point -> Point
advance (Point (x, y) (vX, vY)) = Point (x + vX, y + vY) (vX, vY)

variance :: [Point] -> Int
variance points =
    let distance (x, y) (a, b) = (x - a) ^ 2 + (y - b) ^ 2
        (sX, sY) = foldr (\(Point (x, y) _) (accX, accY) -> (accX + x, accY + y)) (0, 0) points
        l = length points
        mu = (div sX l, div sY l)
    in foldr (\(Point (x, y) _) acc -> distance (x, y) mu + acc) 0 points


display :: [Point] -> String
display points =
    let pointX (Point (x, _) _) = x
        pointY (Point (y, _) _) = y
        minX = minimum $ map pointX points
        minY = minimum $ map pointY points
        maxX = maximum $ map pointX points
        maxY = maximum (map pointY points) + 10
        mp = foldr (\(Point p _) -> HM.insert p '#') HM.empty points
    in unlines $ map (\y -> map (\x -> HM.lookupDefault '.' (x, y) mp) [minX..maxX]) [minY..maxY]

displayMeta :: (Int, [Point]) -> String
displayMeta (seconds, points) = show seconds ++ "\n" ++ display points
    
solve1 :: Input -> String
solve1 points = displayMeta $ go (variance points) 0 points
  where
    go v s points =
        let newPoints = map advance points
            newV = variance newPoints
        in if newV > v then (s, points) else go newV (s + 1) newPoints

main :: IO ()
main = do
    input <- readInput <$> readFile "10.txt"
    putStrLn ("Solutions:\n" ++ solve1 input)
