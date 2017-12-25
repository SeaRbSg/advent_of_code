#!/usr/bin/env runhaskell

{-# OPTIONS_GHC -Wno-type-defaults -Wno-name-shadowing #-}

import Test.HUnit
import System.IO (stderr)
import System.Environment (getEnv)
import Data.List

countIf :: (a -> Bool) -> [a] -> Int
countIf p xs = sum $ fmap (\x -> if p x then 1 else 0) xs

run :: String -> (String -> Bool) -> Int
run input p =
  countIf p $ lines input

isUnique :: [String] -> Bool
isUnique words =
  length words == length (nub words)

isValidA :: String -> Bool
isValidA input =
  isUnique $ words input

isValidB :: String -> Bool
isValidB input =
  not $ isAnagram $ words input

isAnagram :: [String] -> Bool
isAnagram words =
  not (isUnique (fmap sort words))

tests :: Test
tests =
  test
  [ 2                 ~=? countIf even [1..5]
  , 3                 ~=? countIf odd  [1..5]
  --------------------------------------------------
  , ["a","b","c","d"] ~=? words "a b c d"
  --------------------------------------------------
  , True              ~=? isUnique (words "aa bb cc dd ee")
  , True              ~=? isUnique (words "aa bb cc dd aaa")
  , False             ~=? isUnique (words "aa bb cc dd aa")
  --------------------------------------------------
  , False             ~=? isAnagram (words "abcde fghij")
  , True              ~=? isAnagram (words "abcde xyz ecdab")
  , False             ~=? isAnagram (words "a ab abc abd abf abj")
  , False             ~=? isAnagram (words "iiii oiii ooii oooi oooo")
  , True              ~=? isAnagram (words "oiii ioii iioi iiio")
  --------------------------------------------------
  , True              ~=? isValidA "aa bb cc dd ee"
  , True              ~=? isValidA "aa bb cc dd aaa"
  , False             ~=? isValidA "aa bb cc dd aa"
  --------------------------------------------------
  , True              ~=? isValidB "abcde fghij"
  , False             ~=? isValidB "abcde xyz ecdab"
  , True              ~=? isValidB "a ab abc abd abf abj"
  , True              ~=? isValidB "iiii oiii ooii oooi oooo"
  , False             ~=? isValidB "oiii ioii iioi iiio"
  --------------------------------------------------
  , 1                 ~=? run "aa bb cc dd ee"  isValidA
  , 1                 ~=? run "aa bb cc dd aaa" isValidA
  , 0                 ~=? run "aa bb cc dd aa"  isValidA
  ]

runTests :: Test -> IO (Counts, Int)
runTests ts = do
  term <- getEnv "TERM"
  let smart = "dumb" /= term
  runTestText (putTextToHandle stderr smart) ts

main :: IO ()
main = do
  _ <- runTests tests
  input <- readFile "input/04.txt"
  print $ run input isValidA
  print $ run input isValidB
