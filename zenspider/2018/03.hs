import Prelude hiding (lex)
import Data.Char (isDigit)
import Data.List (groupBy, concatMap)
import Utils (occur, minteract)
import qualified Data.Map as M

{-# ANN input' "HLint: ignore Defined but not used" #-}
{-# ANN s'     "HLint: ignore Defined but not used" #-}

type Coord  = (Int, Int)
type CoordT = (Coord, Int)
type Region = (Int, Int, Int, Int, Int)

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
parse s = (read n, read a, read b, read c, read d)
  where [_, n, _, a, _, b, _, c, _, d] = lex s

coords :: Region -> [CoordT]
coords (n,x,y,w,h) = [((a+x,b+y), n) | a <- [1..w], b <- [1..h]]

allCoords :: [Region] -> [CoordT]
allCoords = concatMap coords

mapOverlap :: [Region] -> M.Map Coord Int
mapOverlap = occur . map fst . allCoords

overlapping :: [Region] -> M.Map Coord Int
overlapping = M.filter (>1) . mapOverlap

countOverlap :: [Region] -> Int
countOverlap rs = length $ overlapping rs

problem1 :: String -> String
problem1 = show . countOverlap . map parse . lines

main :: IO ()
main = minteract [problem1]
