import Data.Char (toLower)
import Data.List (minimumBy)
import Data.Function (on)

{-# ANN input "HLint: ignore Defined but not used" #-}

-- TODO: move into my own support module:a
minteract :: [String -> String] -> IO ()
minteract fs =
  do s <- getContents
     each fs s
  where
    each :: [String -> String] -> String -> IO ()
    each [] _     = return ()
    each (f:xs) s = do putStrLn (f s)
                       each xs s

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
              map (\c -> clean $ filter ((/= c) . toLower) s) ['a' .. 'z'])

main :: IO ()
main = minteract [problem1, problem2]

input :: String
input = "dabAcCaCBAcCcaDA"
