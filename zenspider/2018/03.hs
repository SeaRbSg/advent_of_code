import Prelude hiding (lex)
import Data.Char (isDigit)
import Data.List (groupBy, concatMap)
import Utils (occur, minteract)
import qualified Data.Map as M

{-# ANN input' "HLint: ignore Defined but not used" #-}
{-# ANN s'     "HLint: ignore Defined but not used" #-}

input' :: String
input' = "#1 @ 1,3: 4x4\n#2 @ 3,1: 4x4\n#3 @ 5,5: 2x2"
s' :: String
s' = head $ lines input'

xor :: Bool -> Bool -> Bool
xor True  False = True
xor False True  = True
xor _     _     = False

xnor :: Bool -> Bool -> Bool
xnor = xor . not

lex :: String -> [String]
lex = groupBy (\a b -> isDigit a `xnor` isDigit b)

parse :: String -> (Int, Int, Int, Int)
parse s = (read a, read b, read c, read d)
  where [_, _, _, a, _, b, _, c, _, d] = lex s

coords :: (Int, Int, Int, Int) -> [(Int,Int)]
coords (x,y,w,h) = [(a+x,b+y) | a <- [1..w], b <- [1..h]]

allCoords :: String -> [(Int, Int)]
allCoords s = concatMap (coords . parse) $ lines s

overlapping :: String -> Int
overlapping s = length $ M.filter (>1) $ occur $ allCoords s

problem1 :: String -> String
problem1 s = show $ overlapping s

main :: IO ()
main = minteract [problem1]
