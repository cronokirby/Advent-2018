import Data.Array (Array, Ix, (!), (//), bounds, listArray)
import Data.Bits ((.&.), (.|.))
import Data.Char (toUpper)
import Data.Int (Int32)
import Debug.Trace


safeIndex :: Ix i => Array i a -> i -> Maybe a
safeIndex arr i =
    let (mn, mx) = bounds arr
    in if i <= mx && i >= mn then Just (arr ! i) else Nothing


data OpName 
    = ADDR | ADDI | MULR | MULI 
    | BANR | BANI | BORR | BORI
    | SETR | SETI | GTIR | GTRI
    | GTRR | EQIR | EQRI | EQRR
    deriving (Eq, Show, Read)

readOpName :: String -> OpName
readOpName = read . map toUpper


data Op = Op OpName Int32 Int32 Int32 deriving (Eq, Show)

readOp :: String -> Op
readOp str = 
    let (w:ws)    = words str
        [a, b, c] = map read ws
    in Op (readOpName w) a b c


data Registers = Registers (Array Int32 Int32) deriving (Show)

emptyRegisters :: Registers
emptyRegisters = Registers (listArray (0, 5) (repeat 0))

get :: Int32 -> Registers -> Int32
get reg (Registers arr) = arr ! reg

set :: Int32 -> Int32 -> Registers -> Registers
set reg val (Registers arr) = Registers (arr // [(reg, val)])


runOp :: Op -> Registers -> Registers
runOp (Op code a b c) = case code of
    ADDR -> indirect (+) a b c
    ADDI -> direct (+) a b c
    MULR -> indirect (*) a b c
    MULI -> direct (*) a b c
    BANR -> indirect (.&.) a b c
    BANI -> direct (.&.) a b c
    BORR -> indirect (.|.) a b c
    BORI -> direct (.|.) a b c
    SETR -> \regs -> set c (get a regs) regs
    SETI -> \regs -> set c a regs
    GTIR -> directL (cond (>)) a b c
    GTRI -> direct (cond (>)) a b c
    GTRR -> indirect (cond (>)) a b c
    EQIR -> directL (cond (==)) a b c
    EQRI -> direct (cond (==)) a b c
    EQRR -> indirect (cond (==)) a b c
  where
    indirect f a b c regs = set c (f (get a regs) (get b regs)) regs
    direct f a b c regs   = set c (f (get a regs) b) regs
    directL f a b c regs  = set c (f a (get b regs)) regs
    cond f a b = if f a b then 1 else 0


data CPU = CPU Int32 (Array Int32 Op) Registers

readCPU :: String -> CPU
readCPU str =
    let (l:ls) = lines str
        ipReg = read $ words l !! 1
        instructions = listArray (0, fromIntegral $ length ls - 1) (readOp <$> ls)
    in CPU ipReg instructions emptyRegisters

-- | Step the cpu, returning Left
stepCPU :: CPU -> Either CPU CPU
stepCPU cpu@(CPU ipReg arr regs) = 
    case safeIndex arr (get ipReg regs) of
        Nothing -> Left cpu
        Just op -> 
            let regs' = runOp op regs
            in Right $ CPU ipReg arr (set ipReg (get ipReg regs' + 1) regs')

simulate :: CPU -> CPU
simulate = either id simulate . stepCPU

simulateN :: Int -> CPU -> CPU
simulateN 0 = id
simulateN n = either id (simulateN (n - 1)) . stepCPU


solve1 :: CPU -> Int32
solve1 cpu =
    let (CPU _ _ regs) = simulate cpu
    in get 0 regs


solve2 :: CPU -> Int32
solve2 (CPU a b regs) =
    let (CPU _ _ (Registers arr)) = simulateN 1000 (CPU a b (set 0 1 regs))
        n = maximum arr
    in sum $ filter ((== 0) . mod n) [1..n]


main :: IO ()
main = do
    cpu <- readCPU <$> readFile "data/19.txt"
    putStrLn ("Solution 1: " ++ show (solve1 cpu))
    putStrLn ("Solution 2: " ++ show (solve2 cpu))
