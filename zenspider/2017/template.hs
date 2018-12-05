#!/usr/bin/env runhaskell

import Test.HUnit
import System.IO (stderr)
import System.Environment (getEnv)
import Data.Char

problemNNa :: String -> Int
problemNNa a = 42

problemNNb :: String -> Int
problemNNb a = 42

tests :: Test
tests =
  test
    [ 24  ~=? problemNNa "1122"
    ------------------------------
    , 24  ~=? problemNNb "1212" ]

runTests :: Test -> IO (Counts, Int)
runTests ts = do
  term <- getEnv "TERM"
  let smart = "dumb" /= term
  runTestText (putTextToHandle stderr smart) ts

main :: IO ()
main = do
  _ <- runTests tests
  [input] <- lines <$> readFile "input/NN.txt"
  print $ problemNNa input
  print $ problemNNb input
