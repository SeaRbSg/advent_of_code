import Control.Applicative (many)
import Data.Either         (fromRight)
import Data.List           (sort)
import Data.Map            (Map)
import Data.Ord            (comparing)
import Utils               (cleanStr, maximumVal, maximumValBy, minteract, occur)
import qualified Data.Map    as M
import qualified Text.Parsec as P

data Event = G Guard
           | S
           | W
           | E String
           deriving (Show, Eq, Ord)

data Time = T { _y :: Int, _m :: Int, _d :: Int, _h :: Int, _n :: Int }
  deriving (Show, Eq, Ord)

type Guard    = Int
type Minute   = Int
type Log      = (Time, Event)
type TimeCard = Map Minute Int -- min -> count
type Parser   = P.Parsec [Log] ()
type TimeCards = Map Guard TimeCard

parseLine :: String -> Log
parseLine s = (t, e)
  where (ts, rest) = splitAt 5 . words . cleanStr $ s
        [y,m,d,h,m_] = fmap read ts
        t            = T y m d h m_
        e            = case rest of
                         "falls":"asleep":_ -> S
                         "wakes":"up":_     -> W
                         "Guard":n:_        -> G $ read n
                         _                  -> error $ "bad: " ++ s

parseLogs :: [Log] -> TimeCards
parseLogs logs = fromRight M.empty $ P.parse go "" logs
  where
    go :: Parser TimeCards
    go = fmap occur . M.fromListWith (++) <$> many shift

    shift :: Parser (Guard, [Minute])
    shift = do
      (_, G g)   <- P.anyToken
      naps <- concat <$> many (P.try nap)
      return (g, naps)

    nap :: Parser [Minute]
    nap = do
      (T _ _ _ _ m0, S) <- P.anyToken
      (T _ _ _ _ m1, W) <- P.anyToken
      return [m0 .. m1 - 1]

solve1 :: TimeCards -> Int
solve1 tcs = guard * minute
  where
    (guard, tc) = maximumValBy (comparing sum) tcs
    (minute, _) = maximumVal tc

solve2 :: TimeCards -> Int
solve2 tcs = guard * minute
  where
    byMinute = fmap maximumVal tcs
    (guard, (minute, _)) = maximumValBy (comparing snd) byMinute

massage :: String -> TimeCards
massage = M.filter (not . null) . parseLogs . fmap parseLine . sort . lines

problem1 :: String -> String
problem1 = show . solve1 . massage

problem2 :: String -> String
problem2 = show . solve2 . massage

main :: IO ()
main = minteract [problem1, problem2]
