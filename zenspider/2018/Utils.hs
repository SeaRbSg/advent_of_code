module Utils (
  minteract
  ) where

import Data.Foldable (traverse_)

minteract :: [String -> String] -> IO ()
minteract fs =
  do s <- getContents
     traverse_ (putStrLn . ($ s)) fs
