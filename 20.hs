import qualified Data.Set as Set


data Pos = Pos Int Int deriving (Eq, Ord, Show)


data Edge = Edge Pos Pos deriving (Eq, Ord, Show)

makeEdge :: Pos -> Pos -> Edge
makeEdge p1 p2
    | p1 <= p2  = Edge p1 p2
    | otherwise = Edge p2 p1


data Dir = North | South | West | East deriving (Show)

readDir :: Char -> Dir
readDir c = case c of
    'N' -> North
    'S' -> South
    'W' -> West
    'E' -> East
    _   -> error "Invalid direction"

move :: Dir -> Pos -> Pos
move dir = case dir of
    North -> \(Pos x y) -> Pos x (y + 1)
    South -> \(Pos x y) -> Pos x (y - 1)
    East  -> \(Pos x y) -> Pos (x + 1) y
    West  -> \(Pos x y) -> Pos (x - 1) y

adjacent :: Pos -> [Pos]
adjacent pos = map (`move` pos) [North, South, West, East]


data RegTok = RegDir Dir | RegStart | RegEnd | RegOr | RegLPar | RegRPar deriving (Show)

readTok :: Char -> RegTok
readTok c = case c of
    '^' -> RegStart
    '$' -> RegEnd
    '|' -> RegOr
    '(' -> RegLPar
    ')' -> RegRPar
    c   -> RegDir (readDir c)


type Path = [RegTok]

readPath :: String -> Path
readPath = map readTok


pathEdges :: Path -> Set.Set Edge
pathEdges path = go path (Pos 0 0) [] Set.empty
  where
    go [] _ _ set = set
    go (RegStart:ts) pos stack set    = go ts pos stack set
    go (RegEnd:ts) _ _ set            = set
    go (RegLPar:ts) pos stack set     = go ts pos (pos:stack) set
    go (RegOr:ts) _ stack@(pos:_) set = go ts pos stack set
    go (RegRPar:ts) _ (pos:stack) set = go ts pos stack set
    go ((RegDir d):ts) pos stack set  =
        let nextPos = move d pos
            set' = Set.insert (makeEdge pos nextPos) set
        in go ts nextPos stack set'

furthestRoom :: Set.Set Edge -> Int
furthestRoom edges = go 0 Set.empty (Pos 0 0)
  where
    go n seen pos
        | null neighbors = n
        | otherwise      = 
            maximum $ go (n + 1) (Set.insert pos seen) <$> neighbors
      where
        neighbors = 
            filter ((`Set.member` edges) . makeEdge pos) 
            . filter (`Set.notMember` seen) $ adjacent pos

distances :: Set.Set Edge -> [Int]
distances edges = go 0 Set.empty (Pos 0 0)
  where
    go n seen pos = 
        let neighbors =
                filter ((`Set.member` edges) . makeEdge pos)
                . filter (`Set.notMember` seen) $ adjacent pos
        in n : (go (n + 1) (Set.insert pos seen) =<< neighbors)


solve1 :: Set.Set Edge -> Int
solve1 = furthestRoom


solve2 :: Set.Set Edge -> Int
solve2 = length . filter (>= 1000) . distances


main :: IO ()
main = do
    edges <- pathEdges . readPath <$> readFile "data/20.txt"
    putStrLn ("Solution 1: " ++ show (solve1 edges))
    putStrLn ("Solution 2: " ++ show (solve2 edges))
