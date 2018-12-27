{-# LANGUAGE RecordWildCards #-} -- for R {..} -> R { a=a, b=b ... }
{-# OPTIONS_GHC -Wno-type-defaults #-}

import Data.Char
import Data.Bits
import Utils (cleanStr, minteract, splitString, splitEvery, xinteract)
-- for testing:
import Test.HUnit
import System.Environment (getEnv)
import System.IO (stderr)

{-# ANN file' "HLint: ignore Defined but not used" #-}
{-# ANN main' "HLint: ignore Defined but not used" #-}
{-# ANN validate "HLint: ignore Defined but not used" #-}

type Prim     = Int -> Int -> Int
type Op       = Register -> Instruction -> Register

-- NOTE: I'm not sold on Reg as a separate type yet.
data Reg         = R0 | R1 | R2 | R3               deriving (Show, Eq, Ord, Enum)
data Register    = R { r0, r1, r2, r3 :: Int }     deriving (Show, Eq, Ord)
data Instruction = I { op, a, b :: Int, c :: Reg } deriving (Show, Eq)

i2r :: Int -> Reg
i2r = toEnum

get :: Register -> Int -> Int
get R {..} c =                  -- Honestly, code like this seems dumb
  case c of
    0 -> r0
    1 -> r1
    2 -> r2
    3 -> r3
    _ -> error "nope"

set :: Register -> Reg -> Int -> Register
set r c v =                     -- Same... dumb.
  case c of
    R0 -> r { r0 = v }
    R1 -> r { r1 = v }
    R2 -> r { r2 = v }
    R3 -> r { r3 = v }

uprr :: Register -> Instruction -> Prim -> Register
uprr r I {..} fn = set r c (get r a `fn` get r b)

upri :: Register -> Instruction -> Prim -> Register
upri r I {..} fn = set r c (get r a `fn` b)

upir :: Register -> Instruction -> Prim -> Register
upir r I {..} fn = set r c (a `fn` get r b)

addr, addi, mulr, muli, banr, bani, borr, bori,
  setr, seti, gtir, gtri, gtrr, eqir, eqri, eqrr :: Op

addr r o = uprr r o (+)
addi r o = upri r o (+)
mulr r o = uprr r o (*)
muli r o = upri r o (*)
banr r o = uprr r o (.&.)
bani r o = upri r o (.&.)
borr r o = uprr r o (.|.)
bori r o = upri r o (.|.)
setr r o = uprr r o const
seti r o = set r (c o) (a o)
gtir r o = upir r o gti
gtri r o = upri r o gti
gtrr r o = uprr r o gti
eqir r o = upir r o eqi
eqri r o = upri r o eqi
eqrr r o = uprr r o eqi

booli    :: Bool -> Int
booli b   = if b then 1 else 0
gti, eqi :: Int -> Int -> Int
gti a b   = booli $ a > b
eqi a b   = booli $ a == b

ops :: [Op]
ops = [addr, addi, mulr, muli, banr, -- 0 - 4
       bani, borr, bori, setr, seti, -- 5 - 9
       gtir, gtri, gtrr, eqir, eqri, -- A - E
       eqrr]                         -- F

findOps :: Register -> Instruction -> Register -> [Int]
findOps r1 op r2 = fmap snd . filter (\(fn,_) -> fn r1 op == r2) $ zip ops [0..]

countOps :: Register -> Instruction -> Register -> Int
countOps r1 op r2 = length $ findOps r1 op r2

parse :: [String] -> (Register, Instruction, Register)
parse ss = (r, i, e)
  where
    ns :: [Int]
    ns = fmap read $ filter (isDigit . head) ss
    [r0, r1, r2, r3, op, a, b, c, e0, e1, e2, e3] = ns
    r = R r0 r1 r2 r3
    i = I op a b (i2r c)
    e = R e0 e1 e2 e3

solve1 :: (Register, Instruction, Register) -> Int
solve1 (r, e, i) = countOps r e i

problem1 :: String -> String
problem1 s = show . length . filter (> 2) $ fmap (solve1 . parse) tests
  where (ls,_) = splitString "\n\n\n\n" s
        tests = splitEvery 14 . words $ cleanStr ls

main :: IO ()
main = minteract [problem1]

----------------------------------------------------------------------

main' :: IO ()
main' = xinteract [problem1] file'

file' :: String
file' = unlines ["Before: [3, 2, 1, 1]",
                 "9 2 1 2",
                 "After:  [3, 2, 2, 1]",
                 "Before: [1, 1, 0, 1]",
                 "0 1 0 1",
                 "After:  [1, 1, 0, 1]",
                 "Before: [2, 2, 2, 1]",
                 "2 1 2 2",
                 "After:  [2, 2, 1, 1]"
                ]

validate :: IO (Counts, Int)
validate = runTests tests
  where
    runTests ts = do
      term <- getEnv "TERM"
      let smart = "dumb" /= term
      runTestText (putTextToHandle stderr smart) ts
    tests = test [ R3        ~=? i2r 3

                 , 4         ~=? get (R 4 3 2 1) 0
                 , R 4 3 2 0 ~=? set  reg R3 0

                 , R 4 3 2 5 ~=? addr reg (I 42 1 2 R3)
                 , R 4 3 2 5 ~=? addi reg (I 42 1 2 R3)
                 , R 4 3 2 6 ~=? mulr reg (I 42 1 2 R3)
                 , R 4 3 2 6 ~=? muli reg (I 42 1 2 R3)
                 , R 4 3 2 2 ~=? banr reg (I 42 1 2 R3) -- 10 & 11 == 10
                 , R 4 3 2 2 ~=? bani reg (I 42 1 2 R3) -- 10 & 10 == 10
                 , R 4 3 2 3 ~=? borr reg (I 42 1 2 R3) -- 11 | 10 == 11
                 , R 4 3 2 3 ~=? bori reg (I 42 1 2 R3) -- 11 | 10 == 11
                 , R 4 3 2 3 ~=? setr reg (I 42 1 2 R3)
                 , R 4 3 2 1 ~=? seti reg (I 42 1 2 R3)

                 , R 4 3 2 0 ~=? gtir reg (I 42 1 2 R3)
                 , R 4 3 2 1 ~=? gtir reg (I 42 3 2 R3)
                 , R 4 3 2 1 ~=? gtri reg (I 42 1 2 R3)
                 , R 4 3 2 0 ~=? gtri reg (I 42 1 4 R3)
                 , R 4 3 2 1 ~=? gtrr reg (I 42 1 2 R3)
                 , R 4 3 2 0 ~=? gtrr reg (I 42 2 1 R3)

                 , R 4 3 2 0 ~=? eqir reg (I 42 1 2 R3) -- 1 == R2(2) = 0
                 , R 4 3 2 1 ~=? eqir reg (I 42 2 2 R3)

                 , R 4 3 2 0 ~=? eqri reg (I 42 1 2 R3)
                 , R 4 3 2 1 ~=? eqri reg (I 42 1 3 R3)
                 , R 4 3 2 0 ~=? eqrr reg (I 42 1 2 R3)
                 , R 4 3 2 1 ~=? eqrr reg (I 42 1 1 R3)

                 , [1,2,9]   ~=? findOps (R 3 2 1 1) (I 9 2 1 R2) (R 3 2 2 1)
                 , 3         ~=? countOps (R 3 2 1 1) (I 9 2 1 R2) (R 3 2 2 1)

                 -- trying to debug from input file
                 , R 2 2 0 1 ~=? eqir     (R 2 2 2 1) (I 2 1 2 R2)
                 , R 2 2 1 1 ~=? eqri     (R 2 2 2 1) (I 2 1 2 R2)
                 , R 2 2 1 1 ~=? eqrr     (R 2 2 2 1) (I 2 1 2 R2)
                 , [9,14,15] ~=? findOps  (R 2 2 2 1) (I 2 1 2 R2) (R 2 2 1 1)
                 , 3         ~=? countOps (R 2 2 2 1) (I 2 1 2 R2) (R 2 2 1 1)

                 -- back to normal tests
                 , 1         ~=? booli True
                 , 0         ~=? booli False

                 , 0 ~=? gti 1 2
                 , 1 ~=? gti 2 1
                 , 0 ~=? eqi 1 2
                 , 1 ~=? eqi 1 1

                 ]
    reg = R 4 3 2 1
