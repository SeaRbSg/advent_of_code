import Utils (minteract)
import qualified Data.Set as S

parseInt :: String -> Int
parseInt = read . filter (/= '+')

problem1 :: String -> String
problem1 = show . sum . fmap parseInt . lines

second :: Ord a => [a] -> a
second = go S.empty
  where go _    []     = error "Nope!"
        go seen (x:xs) = if x `S.member` seen then x else go (x `S.insert` seen) xs

problem2 :: String -> String
problem2 = show . solve . cycle . fmap parseInt . lines
  where solve ns = second $ sums ns
        sums     = scanl (+) 0

main :: IO ()
main = minteract [problem1, problem2]
