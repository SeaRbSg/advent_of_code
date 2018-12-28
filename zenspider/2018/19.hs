{-# LANGUAGE RecordWildCards #-} -- for R {..} -> R { a=a, b=b ... }

-- import Data.Char
import Data.Bits ((.&.), (.|.))
import Utils
--
-- import qualified Data.List as L
import qualified Data.Map as M

{-# ANN file' "HLint: ignore Defined but not used" #-}
{-# ANN main' "HLint: ignore Defined but not used" #-}

type Prim = Int -> Int -> Int
type Op   = Register -> Instruction -> Register

-- -- NOTE: I'm not sold on Reg as a separate type yet.
data Reg         = R0 | R1 | R2 | R3 | R4 | R5 deriving (Show, Eq, Ord, Enum)
data Register    = R { pc, r0, r1, r2, r3, r4, r5 :: Int } deriving (Show, Eq, Ord)
data Instruction = I { op :: Op, a, b :: Int, c :: Reg } -- deriving (Show, Eq)

newR :: Register
newR = R 0 0 0 0 0 0 0

parseInst :: [String] -> Instruction
parseInst s = I op a b (i2r c)
  where [op', a', b', c'] = s
        [a, b, c] = fmap readi [a', b', c']
        op = ops M.! op'

i2r :: Int -> Reg
i2r c =
  case c of
    0 -> R0
    1 -> R1
    2 -> R2
    3 -> R3
    4 -> R4
    5 -> R5
    _ -> error "nope!"

get :: Register -> Reg -> Int
get R {..} c =                  -- Honestly, code like this seems dumb
  case c of
    R0 -> r0
    R1 -> r1
    R2 -> r2
    R3 -> r3
    R4 -> r4
    R5 -> r5

geti :: Register -> Int -> Int
geti r c = get r (i2r c)

set :: Register -> Reg -> Int -> Register
set r c v =                     -- Same... dumb.
  case c of
    R0 -> r { r0 = v }
    R1 -> r { r1 = v }
    R2 -> r { r2 = v }
    R3 -> r { r3 = v }
    R4 -> r { r4 = v }
    R5 -> r { r5 = v }

uprr :: Register -> Instruction -> Prim -> Register
uprr r I {..} fn = set r c (geti r a `fn` geti r b)

upri :: Register -> Instruction -> Prim -> Register
upri r I {..} fn = set r c (geti r a `fn` b)

upir :: Register -> Instruction -> Prim -> Register
upir r I {..} fn = set r c (a `fn` geti r b)

addr, addi, mulr, muli, banr, bani, borr, bori,
  setr, seti, gtir, gtri, gtrr, eqir, eqri, eqrr :: Op

addr r o = uprr r o (+)      --  1 opcode, see `ins` below
addi r o = upri r o (+)      -- 13
mulr r o = uprr r o (*)      -- 15
muli r o = upri r o (*)      -- 14
banr r o = uprr r o (.&.)    --  0
bani r o = upri r o (.&.)    --  9
borr r o = uprr r o (.|.)    --  8
bori r o = upri r o (.|.)    --  5
setr r o = uprr r o const    --  3
seti r o = set r (c o) (a o) --  7
gtir r o = upir r o gti      --  6
gtri r o = upri r o gti      -- 12
gtrr r o = uprr r o gti      --  4
eqir r o = upir r o eqi      -- 10
eqri r o = upri r o eqi      --  2
eqrr r o = uprr r o eqi      -- 11

booli    :: Bool -> Int
booli b   = if b then 1 else 0
gti, eqi :: Int -> Int -> Int
gti a b   = booli $ a > b
eqi a b   = booli $ a == b

ops :: M.Map String Op
ops = M.fromList [("addr", addr),
       ("addi", addi),
       ("mulr", mulr),
       ("muli", muli),
       ("banr", banr),
       ("bani", bani),
       ("borr", borr),
       ("bori", bori),
       ("setr", setr),
       ("seti", seti),
       ("gtir", gtir),
       ("gtri", gtri),
       ("gtrr", gtrr),
       ("eqir", eqir),
       ("eqri", eqri),
       ("eqrr", eqrr)]

exec :: Register -> Reg -> [Instruction] -> Register
exec reg slot is = go reg
  where
    maxI         = length is
    updateSlot r = set r slot
    updatePC   r = r { pc = get r slot + 1 }
    go r         =
      if pc' >= maxI
        then r
        else let q   = updateSlot r pc' -- copy PC to R0
                 i   = is !! pc'        -- get instruction via pc
                 r'  = op i q i         -- execute instructon
                 r'' = updatePC r'      -- copy R0 to PC & increment PC
              in go r''
      where
        pc' = pc r

munge :: String -> (Int, [Instruction])
munge s = (ip, is)
  where
    ([_, ip'], is') = splitAt 2 $ words s
    ip              = readi ip'
    is''            = splitEvery 4 is'
    is              = fmap parseInst is''

problem1 :: String -> String
problem1 s = show $ exec reg (i2r ip) is
  where
    (ip, is) = munge s
    reg      = newR

problem2 :: String -> String
problem2 s = show $ exec reg (i2r ip) is
  where
    (ip, is) = munge s
    reg      = newR { r0 = 1 }

main :: IO ()
main = minteract [problem1, problem2]

----------------------------------------------------------------------

main' :: IO ()
main' = xinteract [problem1] file'

file' :: String
file' =
  unlines
    [ "#ip 0"
    , "seti 5 0 1"
    , "seti 6 0 2"
    , "addi 0 1 0"
    , "addr 1 2 3"
    , "setr 1 0 0"
    , "seti 8 0 4"
    , "seti 9 0 5"
    ]
