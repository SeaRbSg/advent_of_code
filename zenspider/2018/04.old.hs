{-# OPTIONS_GHC -Wall -Wno-unused-top-binds -Wno-missing-signatures -Wno-name-shadowing -Wno-unused-imports -Wno-type-defaults #-}

import Data.Function (on)
import Text.ParserCombinators.Parsec (Parser)
import Text.Parsec.Prim (ParsecT, parse, (<|>))
import Text.ParserCombinators.Parsec.Language (emptyDef)
import Data.Functor.Identity (Identity)
import qualified Text.ParserCombinators.Parsec.Token as P
import qualified Data.List as L
import qualified Data.Map as M
import Utils (minteract, mmapBy, occur)

import Text.Regex.PCRE
import qualified Data.ByteString.Char8 as BC

type Log       = (Event, DateStamp)
type DateStamp = (Int, Int, Int, Int, Int)

data Event = Guard Int
           | Sleep
           | Awake deriving (Read, Show, Eq)

expand :: [[Log]] -> [[Int]]
expand = fmap topProcess
  where topProcess ((Guard n, _):es) = n : evProcess es
        topProcess (_:_)             = error "no!"
        topProcess []                = error "no!"

        evProcess ((Sleep, d1):(Awake, d2):es) = drange d1 d2 ++ evProcess es
        evProcess []                           = []
        evProcess [_]                          = error "no!"
        evProcess xx@(_:_:_)                   = error $ "no!\n\n" ++ show xx

        drange (_,_,_,_,m1) (_,_,_,_,m2) = L.init [m1..m2]

parseAll :: [String] -> [Log]
parseAll = fmap go
  where go str =
          case parse logParser "" str of
            Left err  -> error $ "parse error at " ++ show err
            Right val -> val

groupShifts :: [Log] -> [[Log]]
groupShifts = L.groupBy go'
  where go' _ b =
          case b of
            (Guard _, _) -> False
            _            -> True

solve1 :: [Log] -> Int
solve1 xs = laziest * mostMin
  where shifts   = expand . groupShifts $ xs
        byShift  = M.map (L.sort . concat) . mmapBy head tail $ shifts
        laziest  = biggestV . M.toList . M.map length $ byShift
        minutes  = L.sort $ byShift M.! laziest
        mostMin  = biggestV . M.toList $ occur minutes
        biggestV = fst . L.maximumBy (compare `on` snd)

-- a <- readFile "04.txt"

-- let m = M.map (L.sort . concat) $ mmapBy head tail $ expand . groupShifts . parseAll . L.sort . lines $ a
-- L.maximumBy (compare `on` snd) $ M.toList $ occur $ m M.! 1307

-- junk?
-- L.maximumBy (compare `on` snd) $ M.toList $ m M.! 1307
-- L.maximumBy (compare `on` snd) $ concat $ map M.toList $ M.elems $ M.map (occur . concat) $ mmapBy head tail $ expand . groupShifts . parseAll . L.sort . lines $ log'

solve2 :: [Log] -> Int
solve2 xs = laziest * mostMin
  where shifts   = expand . groupShifts $ xs
        byShift  = M.map (L.sort . concat) . mmapBy head tail $ shifts
        laziest  = biggestV . M.toList . M.map length $ byShift
        minutes  = L.sort $ byShift M.! laziest
        mostMin  = biggestV . M.toList $ occur minutes
        biggestV = fst . L.maximumBy (compare `on` snd)

problem1 :: String -> String
problem1 = show . solve1 . parseAll . L.sort . lines

problem2 :: String -> String
problem2 = show . solve2 . parseAll . L.sort . lines

main :: IO ()
main = minteract [problem1, problem2]

------------------------------------------------------------------------------
-- Parser

logParser    :: Parser Log
dateParser   :: Parser DateStamp
entryParser  :: Parser Event
beginParser  :: Parser Event
asleepParser :: Parser Event
awakeParser  :: Parser Event
lexer        :: P.GenTokenParser String u0 Identity
symbol       :: String -> ParsecT String u Identity String
ws           :: ParsecT String u Identity ()
number       :: ParsecT String u Identity Integer
brackets     :: ParsecT String u Identity a -> ParsecT String u Identity a

logParser = do
  d <- brackets dateParser
  e <- entryParser
  return (e, d)

dateParser = do
  yyyy <- number
  _    <- symbol "-"
  mm   <- number
  _    <- symbol "-"
  dd   <- number
  _    <- ws
  hh   <- number
  _    <- symbol ":"
  mn   <- number
  return (rs yyyy, rs mm, rs dd, rs hh, rs mn) -- HACK!
  where rs = read . show                       -- HACK!!

entryParser = beginParser <|> asleepParser <|> awakeParser

beginParser = do
  _ <- symbol "Guard #"
  n <- number
  _ <- symbol "begins shift"
  return . Guard $ fromIntegral n

asleepParser = do
  _ <- symbol "falls asleep"
  return Sleep

awakeParser = do
  _ <- symbol "wakes up"
  return Awake


lexer    = P.makeTokenParser emptyDef
symbol   = P.symbol     lexer
ws       = P.whiteSpace lexer
number   = P.integer    lexer
brackets = P.brackets   lexer

------------------------------------------------------------------------------
-- Junk:

log' :: String
log' = unlines [ "[1518-11-01 00:00] Guard #10 begins shift",
                 "[1518-11-01 00:05] falls asleep",
                 "[1518-11-01 00:25] wakes up",
                 "[1518-11-01 00:30] falls asleep",
                 "[1518-11-01 00:55] wakes up",
                 "[1518-11-01 23:58] Guard #99 begins shift",
                 "[1518-11-02 00:40] falls asleep",
                 "[1518-11-02 00:50] wakes up",
                 "[1518-11-03 00:05] Guard #10 begins shift",
                 "[1518-11-03 00:24] falls asleep",
                 "[1518-11-03 00:29] wakes up",
                 "[1518-11-04 00:02] Guard #99 begins shift",
                 "[1518-11-04 00:36] falls asleep",
                 "[1518-11-04 00:46] wakes up",
                 "[1518-11-05 00:03] Guard #99 begins shift",
                 "[1518-11-05 00:45] falls asleep",
                 "[1518-11-05 00:55] wakes up"]

shifts' =
  [ [ (Guard 10, (1518, 11, 1, 0, 0))
    , (Sleep,    (1518, 11, 1, 0, 5))
    , (Awake,    (1518, 11, 1, 0, 25))
    , (Sleep,    (1518, 11, 1, 0, 30))
    , (Awake,    (1518, 11, 1, 0, 55))
    ]
  , [ (Guard 99, (1518, 11, 1, 23, 58))
    , (Sleep,    (1518, 11, 2, 0, 40))
    , (Awake,    (1518, 11, 2, 0, 50))
    ]
  , [ (Guard 10, (1518, 11, 3, 0, 5))
    , (Sleep,    (1518, 11, 3, 0, 24))
    , (Awake,    (1518, 11, 3, 0, 29))
    ]
  , [ (Guard 99, (1518, 11, 4, 0, 2))
    , (Sleep,    (1518, 11, 4, 0, 36))
    , (Awake,    (1518, 11, 4, 0, 46))
    ]
  , [ (Guard 99, (1518, 11, 5, 0, 3))
    , (Sleep,    (1518, 11, 5, 0, 45))
    , (Awake,    (1518, 11, 5, 0, 55))
    ]
  ]
