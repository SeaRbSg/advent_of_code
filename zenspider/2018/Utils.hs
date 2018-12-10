{-# LANGUAGE TupleSections #-}

module Utils
  ( minteract
  , mmapBy
  , occur
  ) where

import Data.Foldable (traverse_)
import Data.Map (Map)
import qualified Data.Map as M

minteract :: [String -> String] -> IO ()
minteract fs =
  do s <- getContents
     traverse_ (putStrLn . ($ s)) fs

occur :: Ord a => [a] -> Map a Int
occur = M.fromListWith (+) . fmap (,1)

mmapBy :: Ord k => (b -> k) -> (b -> a) -> [b] -> Map k [a]
mmapBy kf af = foldl go M.empty
  where go m b = M.insertWith (++) (kf b) [af b] m
