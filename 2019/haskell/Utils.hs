module Utils (extractJoin) where

-- grab the common prefix (in reverse), and the two divergent suffices (forwards)
extractJoin :: Eq a => [a] -> [a] -> ([a],[a],[a])
extractJoin [] bs = ([],[],bs)
extractJoin as [] = ([],as,[])
extractJoin (a:as) (b:bs) | a == b = let (p,l,r) = extractJoin as bs
                                     in (a:p,l,r)
                          | otherwise = ([],a:as,b:bs)
