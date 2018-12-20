import Data.Array (Array, (!), (//), elems, listArray)
import Data.Bits ((.&.), (.|.))
import Text.ParserCombinators.ReadP (ReadP, char, many1, readP_to_S, readS_to_P, skipSpaces, string)


newtype Registers = Registers (Array Int Int) deriving (Eq)

instance Show Registers where
    show (Registers arr) = "Registers " ++ show (elems arr)

registersFromList :: [Int] -> Registers
registersFromList = Registers . listArray (0, 3)

get :: Int -> Registers -> Int
get reg (Registers arr) = arr ! reg

set :: Int -> Int -> Registers -> Registers
set reg val (Registers arr) = Registers (arr // [(reg, val)])


type OpFunc = Int -> Int -> Int -> Registers -> Registers

ops :: [OpFunc]
ops = 
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


data Op = Op { opCode :: Int, opA :: Int, opB :: Int, opC :: Int } deriving (Show)


runOp :: Op -> OpFunc -> Registers -> Registers
runOp (Op _ a b c) f = f a b c


data TestCase = TestCase { caseBefore :: Registers, caseAfter :: Registers } deriving (Show)

behavesLike :: Op -> TestCase -> OpFunc -> Bool
behavesLike op (TestCase before after) f = runOp op f before == after


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
        string "Before: "
        before <- readRegisters
        op <- readOp
        skipSpaces
        string "After:  "
        after  <- readRegisters
        return (op, TestCase before after)

type Input2 = [Op]

readInput2 :: String -> Input2
readInput2 = fst . last . readP_to_S many1 readOp
    

solve1 :: Input1 -> Int
solve1 = length . filter ((>= 3) . length . uncurry matches)
  where
    matches op test = filter (behavesLike op test) ops


main :: IO ()
main = do
    input1 <- readInput1 <$> readFile "data/16-1.txt"
    putStrLn ("Solution 1: " ++ show (solve1 input1))
