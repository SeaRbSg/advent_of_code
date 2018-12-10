import Prelude hiding (lex, all)
import Data.Char (isDigit)
import qualified Data.List as L
import Utils (minteract, mmapBy, occur)
import Data.Map (Map)
import qualified Data.Map as M

type Coord  = (Int, Int)
type CoordT = (Int, Coord)
type Job    = Int
type Region = (Job, Int, Int, Int, Int)

xor :: Bool -> Bool -> Bool
xor True  False = True
xor False True  = True
xor _     _     = False

xnor :: Bool -> Bool -> Bool
xnor = xor . not

lex :: String -> [String]
lex = L.groupBy (\a b -> isDigit a `xnor` isDigit b)

parse :: String -> Region
parse s = (read n, read a, read b, read c, read d)
  where [_, n, _, a, _, b, _, c, _, d] = lex s

coords :: Region -> [CoordT]
coords (n,x,y,w,h) = [(n, (a+x,b+y)) | a <- [1..w], b <- [1..h]]

allCoords :: [Region] -> [CoordT]
allCoords = L.concatMap coords

mapOverlap :: [Region] -> Map Coord Int
mapOverlap = occur . fmap snd . allCoords

overlapping :: [Region] -> Map Coord Int
overlapping = M.filter (>1) . mapOverlap

countOverlap :: [Region] -> Int
countOverlap rs = length $ overlapping rs

findNonOverlap :: [Region] -> [Job]
findNonOverlap rs = M.keys solo
  where solo      = M.filter allUnique jobs
        allUnique = L.all unique
        unique c' = 1 == length (byCoords M.! c')
        jobs      = mmapBy fst snd all -- jobId -> [coord]
        byCoords  = mmapBy snd fst all -- coord -> [jobId]
        all       = allCoords rs

problem1 :: String -> String
problem1 = show . countOverlap . fmap parse . lines

problem2 :: String -> String
problem2 = show . findNonOverlap . fmap parse . lines

main :: IO ()
main = minteract [problem1, problem2]
