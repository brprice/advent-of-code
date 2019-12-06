module Main where

import qualified Data.Map as M

main :: IO ()
main = do orbits <- map (fmap tail . span (/=')')) <$> lines <$> readFile "../data/day6"
          let tree = M.fromList
                   $ ("COM",0)
                   : [(child,l+1 :: Integer) | (parent,child) <- orbits, let l = tree M.! parent]
          putStr "part a: total orbits: "
          print $ sum $ M.elems tree
