import Prelude hiding (lex)
import Data.Char (isDigit)
import Data.List (groupBy, concatMap)
import Utils (occur, minteract)
import qualified Data.Map as M

{-# ANN input' "HLint: ignore Defined but not used" #-}
{-# ANN s'     "HLint: ignore Defined but not used" #-}

type Coord  = (Int, Int)
type Region = (Int, Int, Int, Int)

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

parse :: String -> Region
parse s = (read a, read b, read c, read d)
  where [_, _, _, a, _, b, _, c, _, d] = lex s

coords :: Region -> [Coord]
coords (x,y,w,h) = [(a+x,b+y) | a <- [1..w], b <- [1..h]]

allCoords :: [Region] -> [Coord]
allCoords = concatMap coords

overlapping :: [Region] -> Int
overlapping = length . M.filter (>1) . occur . allCoords

problem1 :: String -> String
problem1 = show . overlapping . map parse . lines

main :: IO ()
main = minteract [problem1]
