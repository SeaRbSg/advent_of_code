#!/usr/bin/env runhaskell

import Test.HUnit
import System.IO (stderr)
import System.Environment (getEnv)
import Data.Char

problem01 :: String -> ([Int] -> Int) -> Int
problem01 input offset_proc =
  sum $ zipWith (curry f) a b
  where
    a = fmap digitToInt input
    b = drop (offset_proc a) (a ++ a)
    f (m, n) = if m == n
               then m
               else 0

problem01a :: String -> Int
problem01a input = problem01 input one
  where one _ = 1

problem01b :: String -> Int
problem01b input = problem01 input half
  where half l = length l `div` 2

tests :: Test
tests =
  test
    [ 3  ~=? problem01a "1122"
    , 4  ~=? problem01a "1111"
    , 0  ~=? problem01a "1234"
    , 9  ~=? problem01a "91212129"
    ------------------------------
    , 6  ~=? problem01b "1212"
    , 0  ~=? problem01b "1221"
    , 4  ~=? problem01b "123425"
    , 12 ~=? problem01b "123123"
    , 4  ~=? problem01b "12131415" ]

runTests :: Test -> IO (Counts, Int)
runTests ts = do
  term <- getEnv "TERM"
  let smart = "dumb" /= term
  runTestText (putTextToHandle stderr smart) ts

main :: IO ()
main = do
  _ <- runTests tests
  [input] <- lines <$> readFile "input/01.txt"
  print $ problem01a input
  print $ problem01b input
