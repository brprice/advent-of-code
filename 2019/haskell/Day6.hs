module Main where

import qualified Data.Map as M

-- grab the common prefix (in reverse), and the two divergent suffices (forwards)
extractJoin :: Eq a => [a] -> [a] -> ([a],[a],[a])
extractJoin [] bs = ([],[],bs)
extractJoin as [] = ([],as,[])
extractJoin (a:as) (b:bs) | a == b = let (p,l,r) = extractJoin as bs
                                     in (a:p,l,r)
                          | otherwise = ([],a:as,b:bs)

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
