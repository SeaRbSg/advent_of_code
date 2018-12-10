{-# LANGUAGE TupleSections #-}

module Utils
  ( cleanStr
  , maximumVal
  , maximumValBy
  , minteract
  , mmapBy
  , occur
  , replace
  , xinteract
  ) where

import Data.Char (isAlphaNum)
import Data.Foldable (traverse_)
import Data.Map (Map)
import Data.List (maximumBy)
import Data.Function (on)
import qualified Data.Map as M

-- replace all non-alphanums with spaces. Combined with `words` is a nice cheat.
cleanStr :: String -> String
cleanStr = replace (not . isAlphaNum) ' '

-- return `(k, v)` for maximum `v`.
maximumVal :: Ord a => Map k a -> (k, a)
maximumVal = maximumValBy compare

-- return `(k, v)` for maximum value using `c` to compare.
maximumValBy :: (a -> a -> Ordering) -> Map k a -> (k, a)
maximumValBy c = maximumBy (c `on` snd) . M.toList

-- like interact, but for many processors
minteract :: [String -> String] -> IO ()
minteract fs =
  do s <- getContents
     xinteract fs s

-- for generating multi-maps with mutations & merging
mmapBy :: Ord k => (b -> k) -> (b -> a) -> [b] -> Map k [a]
mmapBy kf af = foldl go M.empty
  where go m b = M.insertWith (++) (kf b) [af b] m

-- one of my usual tools. Counts occurances. Should return a descending list.
occur :: Ord a => [a] -> Map a Int
occur = M.fromListWith (+) . fmap (,1)

-- map and replace with default value `r` if `p` is true for each `x`
replace :: (a -> Bool) -> a -> [a] -> [a]
replace p r = fmap (\x -> if p x then r else x)

-- for going against internal data until ready to switch to minteract
xinteract :: [String -> String] -> String -> IO ()
xinteract fs s = traverse_ (putStrLn . ($ s)) fs
