#!/usr/bin/env runhaskell

import Test.HUnit
import System.IO (stderr)
import System.Environment (getEnv)
-- import Data.Char

-- cpu = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

-- data LispVal = Atom String
--              | List [ LispVal ]
--              | DottedList [ LispVal ] LispVal
--              | Number Integer
--              | String String
--              | Bool Bool

data Value = Register Char
           | Int Integer

x :: Value
x = Int 42

y :: Value
y = Register 'a'

data Slot = PC Integer
          | Counter Integer
          | A Integer
          | B Integer
          | C Integer
          | D Integer
          | E Integer
          | F Integer
          | G Integer
          | H Integer

-- Num a => (a, [Char])

-- data CPU :: Integer pc, Integer count, Slot reg => (pc, count, reg, reg, reg, reg, reg, reg, reg, reg)

problem23a :: String -> Int
problem23a _ = 42

problem23b :: String -> Int
problem23b _ = 42

tests :: Test
tests =
  test
    [ 24  ~=? problem23a "1122"
    ------------------------------
    , 24  ~=? problem23b "1212" ]

runTests :: Test -> IO (Counts, Int)
runTests ts = do
  term <- getEnv "TERM"
  let smart = "dumb" /= term
  runTestText (putTextToHandle stderr smart) ts

main :: IO ()
main = do
  _ <- runTests tests
  [input] <- lines <$> readFile "input/23.txt"
  print $ problem23a input
  print $ problem23b input


-- fuuuuck you
-- main = print $ length $ filter (not.isPrime.head) $ chunksOf 17 [b..b+17000] where b = 108100


-- data Ins reg imm
--   = Set {reg :: reg, val :: Either reg imm}
--   | Sub {reg :: reg, val :: Either reg imm}
--   | Mul {reg :: reg, val :: Either reg imm}
--   | Jnz {cnd :: Either reg imm, jmp :: Either reg imm}
-- data State a reg imm = State {pc :: Int, regs :: a reg imm}
--
-- parseVal :: (Read imm) => String -> Either Char imm
-- parseVal (readMaybe -> Just imm) = Right imm
-- parseVal [reg] = Left reg
--
-- parse :: (Read imm) => String -> [Ins Char imm]
-- parse = map (parseIns . words) . lines where
--     parseIns ["set", [reg], parseVal -> val] = Set {..}
--     parseIns ["sub", [reg], parseVal -> val] = Sub {..}
--     parseIns ["mul", [reg], parseVal -> val] = Mul {..}
--     parseIns ["jnz", parseVal -> cnd, parseVal -> jmp] = Jnz {..}
--
-- step :: (IArray a imm, Ix reg, Integral imm) =>
--     [Ins reg imm] -> State a reg imm -> Maybe (State a reg imm)
-- step ins State {pc} | pc < 0 || null (drop pc ins) = Nothing
-- step ins state@State {..} = Just $ case ins !! pc of
--     Set {..} -> State (pc + 1) (regs // [(reg, load val)])
--     Sub {..} -> State (pc + 1) (regs // [(reg, regs ! reg - load val)])
--     Mul {..} -> State (pc + 1) (regs // [(reg, regs ! reg * load val)])
--     Jnz {..}
--       | load cnd == 0 -> state {pc = pc + 1}
--       | otherwise -> state {pc = pc + fromIntegral (load jmp)}
--   where
--     load = either (regs !) id
--
-- iterateMaybe :: (a -> Maybe a) -> Maybe a -> [a]
-- iterateMaybe f = maybe [] $ (:) <*> iterateMaybe f . f
--
-- day23a :: String -> Int
-- day23a input = countMuls $ iterateMaybe (step ins) $ Just state0 where
--     ins = parse input
--     muls = findIndices (\case Mul {} -> True; _ -> False) ins
--     countMuls = length . filter (`elem` muls) . map pc
--     state0 = State {pc = 0, regs = listArray @UArray @Int ('a', 'h') $ repeat 0}

-- pt 2:
-- length $ filter (not . isPrime) [107900, 107917 .. 124900]
--
-- or:

-- stepOptimized :: (IArray a imm, Integral imm) =>
--     [Ins Char imm] -> State a Char imm -> Maybe (State a Char imm)
-- stepOptimized ins State {pc} | pc < 0 = Nothing
-- stepOptimized ins state@State {..} = case drop pc ins of
--     ( Set 'd' (Right 2) :
--       Set 'e' (Right 2) :
--       Set 'g' (Left 'd') :
--       Mul 'g' (Left 'e') :
--       Sub 'g' (Left 'b') :
--       Jnz (Left 'g') (Right 2) :
--       Set 'f' (Right 0) :
--       Sub 'e' (Right (-1)) :
--       Set 'g' (Left 'e') :
--       Sub 'g' (Left 'b') :
--       Jnz (Left 'g') (Right (-8)) :
--       Sub 'd' (Right (-1)) :
--       Set 'g' (Left 'd') :
--       Sub 'g' (Left 'b') :
--       Jnz (Left 'g') (Right (-13)) :
--       _) -> Just . State (pc + 15) $ regs //
--             ( [ ('d', regs ! 'b'), ('e', regs ! 'b'), ('g', 0) ] ++
--               [('f', 0) | not . isPrime . fromIntegral $ regs ! 'b'] )
--     _ -> step ins state
--
-- day23b :: String -> Int
-- day23b input =
--     regs (last . iterateMaybe (stepOptimized ins) $ Just state0) ! 'h' where
--     ins = parse input
--     state0 =
--         State {pc = 0, regs = listArray @UArray @Int ('a', 'h') $ 1 : repeat 0}

-- also: https://github.com/NeilNjae/advent-of-code-17/blob/master/src/advent23/advent23.hs
