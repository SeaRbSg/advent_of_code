import Data.Char (toLower)
import Data.List (minimumBy)
import Data.Function (on)
import Utils (minteract)

{-# ANN input' "HLint: ignore Defined but not used" #-}

clean :: String -> String
clean = foldr go ""
  where go a []    = [a]
        go a (b:s) = if bad a b then s else a : b : s
        bad a b    = a /= b && toLower a == toLower b

dispatch :: (String -> String) -> (String -> String)
dispatch f = show . length . f . head . words

problem1 :: String -> String
problem1 = dispatch clean

problem2 :: String -> String
problem2 =
  dispatch (\s ->
              minimumBy (compare `on` length) $
              fmap (\c -> clean $ filter ((/= c) . toLower) s) ['a' .. 'z'])

main :: IO ()
main = minteract [problem1, problem2]

input' :: String
input' = "dabAcCaCBAcCcaDA"
