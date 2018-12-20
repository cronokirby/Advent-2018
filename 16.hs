import Data.Array (Array, (!), (//), accum, assocs, listArray)
import Data.Bits ((.&.), (.|.))
import Data.Foldable (find)
import Data.List (foldl')
import Data.Set (Set)
import qualified Data.Set as Set
import Text.ParserCombinators.ReadP (ReadP, many1, readP_to_S, readS_to_P, skipSpaces, string)


newtype Registers = Registers (Array Int Int) deriving (Eq)

registersFromList :: [Int] -> Registers
registersFromList = Registers . listArray (0, 3)

get :: Int -> Registers -> Int
get reg (Registers arr) = arr ! reg

set :: Int -> Int -> Registers -> Registers
set reg val (Registers arr) = Registers (arr // [(reg, val)])


data OpFunc = OpFunc Int (Int -> Int -> Int -> Registers -> Registers)

instance Eq OpFunc where
    (OpFunc a _) == (OpFunc b _) = a == b

instance Ord OpFunc where
    compare (OpFunc a _ ) (OpFunc b _) = compare a b


ops :: [OpFunc]
ops = zipWith OpFunc [0..]
    [ indirect (+)
    , direct (+)
    , indirect (*)
    , direct (*)
    , indirect (.&.)
    , direct (.&.)
    , indirect (.|.)
    , direct (.|.)
    , \a _ c regs -> set c (get a regs) regs
    , \a _ c regs -> set c a regs
    , directL (cond (>))
    , direct (cond (>))
    , indirect (cond (>))
    , directL (cond (==))
    , direct (cond (==))
    , indirect (cond (==))
    ]
  where
    indirect f a b c regs = set c (f (get a regs) (get b regs)) regs
    direct f a b c regs   = set c (f (get a regs) b) regs
    directL f a b c regs  = set c (f a (get b regs)) regs
    cond f a b = if f a b then 1 else 0


data Op = Op { opCode :: Int, opA :: Int, opB :: Int, opC :: Int }

runOp :: Op -> OpFunc -> Registers -> Registers
runOp (Op _ a b c) (OpFunc _ f) = f a b c


data TestCase = TestCase Registers Registers

behavesLike :: Op -> TestCase -> OpFunc -> Bool
behavesLike op (TestCase before after) f = runOp op f before == after

matches :: Op -> TestCase -> [OpFunc]
matches op test = filter (behavesLike op test) ops


type Input1 = [(Op, TestCase)]

readOp :: ReadP Op
readOp = Op <$> readInt <*> readInt <*> readInt <*> readInt
  where
    readInt :: ReadP Int
    readInt = skipSpaces *> readS_to_P reads

readInput1 :: String -> Input1
readInput1 = fst . last . readP_to_S (many1 parser)
  where
    readRegisters :: ReadP Registers
    readRegisters = registersFromList <$> readS_to_P reads
    parser :: ReadP (Op, TestCase)
    parser = do
        skipSpaces
        _ <- string "Before: "
        before <- readRegisters
        op     <- readOp
        skipSpaces
        _ <- string "After:  "
        after  <- readRegisters
        return (op, TestCase before after)

type Input2 = [Op]

readInput2 :: String -> Input2
readInput2 = fst . last . readP_to_S (many1 readOp)
    

solve1 :: Input1 -> Int
solve1 = length . filter ((>= 3) . length . uncurry matches)


solve2 :: Input1 -> Input2 -> Int
solve2 opCases toRun = 
    let opSets = map (\(op, test) -> (op, Set.fromList (matches op test))) opCases
        arr = makeCodeMap opSets
    in get 0 $ foldl' (runOpCode arr) (registersFromList (repeat 0)) toRun
  where
    makeCodeMap :: [(Op, Set OpFunc)] -> Array Int OpFunc
    makeCodeMap opSets = 
        let allOps = Set.fromList ops 
            start  = listArray (0, 15) (repeat allOps)
            acc    = accum Set.intersection start $ map (\(op, f) -> (opCode op, f)) opSets
        in head . Set.toList <$> eliminate Set.empty acc
    runOpCode :: Array Int OpFunc -> Registers -> Op -> Registers
    runOpCode arr regs op = runOp op (arr ! opCode op) regs
    eliminateIx :: Array Int (Set OpFunc) -> Int -> Array Int (Set OpFunc) 
    eliminateIx arr i = 
        let pairs = zip ([0..i-1] ++ [i+1..15]) (repeat (arr ! i))
        in accum Set.difference arr pairs
    eliminate :: Set Int -> Array Int (Set OpFunc) -> Array Int (Set OpFunc)
    eliminate crossed arr = case find ((&&) <$> notCrossed <*> justOne) (assocs arr) of
        Just (i, _) -> eliminate (Set.insert i crossed) (eliminateIx arr i)
        Nothing     -> arr
      where
        notCrossed (i, _) = Set.notMember i crossed
        justOne (_, s) = Set.size s == 1


main :: IO ()
main = do
    input1 <- readInput1 <$> readFile "data/16-1.txt"
    putStrLn ("Solution 1: " ++ show (solve1 input1))
    input2 <- readInput2 <$> readFile "data/16-2.txt"
    putStrLn ("Solution 2: " ++ show (solve2 input1 input2))
