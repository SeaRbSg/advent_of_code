{-# OPTIONS_GHC -Wall -Wno-unused-top-binds -Wno-missing-signatures -Wno-name-shadowing -Wno-unused-imports -Wno-type-defaults #-}

import Utils

-- like words, but converts to Ints
ints :: String -> [Int]
ints s = fmap read $ words s

parse s = fmap (ints . cleanStr) $ lines s

solve1 = undefined

solve2 = undefined

problem1 = undefined

problem2 = undefined

main = xinteract [problem1, problem2] input

--

input = unlines ["1, 1",
                 "1, 6",
                 "8, 3",
                 "3, 4",
                 "5, 5",
                 "8, 9"]
