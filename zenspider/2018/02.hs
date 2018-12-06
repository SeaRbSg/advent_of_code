import Utils (minteract, occur)

{-# ANN s1 "HLint: ignore Defined but not used" #-}
{-# ANN s2 "HLint: ignore Defined but not used" #-}

s1 :: [String]
s1 = ["abcdef", "bababc", "abbcde", "abcccd", "aabcdd", "abcdee", "ababab"]
s2 :: [String]
s2 = ["abcde", "fghij", "klmno", "pqrst", "fguij", "axcye", "wvxyz"]

one :: Bool -> Int
one b'     = if b' then 1 else 0 :: Int

solve1 :: [String] -> Int
solve1 ss = count (==2) letters * count (==3) letters
  where letters    = fmap occur ss
        count p xs = sum $ fmap (one . any p) xs

problem1 :: String -> String
problem1 = show . solve1 . lines

solve2 :: [String] -> [String]
solve2 ss = [ fmap fst $ filter (uncurry (==)) $ a `zip` b
            | a <- ss
            , b <- ss
            , a > b
            , 1 == sum (fmap (\(c, d) -> one $ c /= d) (a `zip` b))]

problem2 :: String -> String
problem2 = show . solve2 . lines

main :: IO ()
main = minteract [problem1, problem2]
