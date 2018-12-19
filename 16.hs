import Data.Array (Array, (!), (//), elems, listArray)
import Data.Bits ((.&.), (.|.))


data Registers = Registers (Array Int Int) deriving (Eq)

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


data Op = Op { opCode :: Int, opA :: Int, opB :: Int, opC :: Int }


runOp :: Op -> OpFunc -> Registers -> Registers
runOp (Op _ a b c) f = f a b c


data TestCase = TestCase { caseBefore :: Registers, caseAfter :: Registers }

behavesLike :: Op -> TestCase -> OpFunc -> Bool
behavesLike op (TestCase before after) f = runOp op f before == after


type Input1 = [(Op, TestCase)]


solve1 :: Input1 -> Int
solve1 = length . filter ((>= 3) . length . uncurry matches)
  where
    matches op test = filter (behavesLike op test) ops
