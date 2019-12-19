module Main where

import Utils (extractJoin)

import qualified Data.Map as M

main :: IO ()
main = do orbits <- map (fmap tail . span (/=')')) <$> lines <$> readFile "../data/day6"
          let tree = M.fromList
                   $ ("COM",(0,[]))
                   : [(child,(l+1 :: Integer,parent:ppath)) | (parent,child) <- orbits, let (l,ppath) = tree M.! parent]
          putStr "part a: total orbits: "
          print $ sum $ map fst $ M.elems tree

          let santaPath = reverse $ snd $ tree M.! "SAN"
          let youPath = reverse $ snd $ tree M.! "YOU"
          let (_,santa,you) = extractJoin santaPath youPath
          putStr "part b: total transfers to reach santa: "
          print $ length santa + length you
