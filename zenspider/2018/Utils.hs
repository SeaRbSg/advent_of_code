module Utils (
  minteract
  ) where

minteract :: [String -> String] -> IO ()
minteract fs =
  do s <- getContents
     each fs s
  where
    each :: [String -> String] -> String -> IO ()
    each [] _     = return ()
    each (f:xs) s = do putStrLn (f s)
                       each xs s
