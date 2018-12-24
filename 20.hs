import qualified Data.Set as Set


data Pos = Pos Int Int deriving (Eq, Ord, Show)


data Edge = Edge Pos Pos deriving (Eq, Ord, Show)

makeEdge :: Pos -> Pos -> Edge
makeEdge p1 p2
    | p1 <= p2  = Edge p1 p2
    | otherwise = Edge p2 p1


data Dir = North | South | West | East

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

edgeTo :: Dir -> Pos -> Edge
edgeTo dir pos = makeEdge pos (move dir pos)


data RegTok = RegDir Dir | RegStart | RegEnd | RegOr | RegLPar | RegRPar

readTok :: Char -> RegTok
readTok c = case c of
    '^' -> RegStart
    '$' -> RegEnd
    '|' -> RegOr
    '(' -> RegLPar
    ')' -> RegRPar
    c   -> RegDir (readDir c)


type Path = [RegTok]


