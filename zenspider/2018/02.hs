import Utils (minteract, occur)
import Control.Monad (guard)

{-# ANN s1 "HLint: ignore Defined but not used" #-}
{-# ANN s2 "HLint: ignore Defined but not used" #-}

s1 :: [String]
s1 = ["abcdef", "bababc", "abbcde", "abcccd", "aabcdd", "abcdee", "ababab"]
s2 :: [String]
s2 = ["abcde", "fghij", "klmno", "pqrst", "fguij", "axcye", "wvxyz"]

one :: Bool -> Int
one b'     = if b' then 1 else 0

solve1 :: [String] -> Int
solve1 ss = count (==2) letters * count (==3) letters
  where letters    = fmap occur ss
        count p xs = sum $ fmap (one . any p) xs

problem1 :: String -> String
problem1 = show . solve1 . lines

solve2 :: [String] -> [String]
solve2 ss = [ c
            | a <- ss
            , b <- ss
            , a > b
            , let c = fmap fst $ filter (uncurry (==)) $ a `zip` b
            , length a - length c == 1
            ]

solve3 :: [String] -> [String]
solve3 ss = do a <- ss
               b <- ss
               guard $ a > b
               let c = fmap fst $ filter (uncurry (==)) $ a `zip` b
               guard $ length a - length c == 1
               pure c

problem2 :: String -> String
problem2 = head . solve2 . lines

problem3 :: String -> String
problem3 = head . solve3 . lines

main :: IO ()
main = minteract [problem1, problem2, problem3]
