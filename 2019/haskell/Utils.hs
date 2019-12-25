module Utils (V2, (.+), (.-), neighboursGrid, parseCharArray
             ,dijkstra
             ,extractJoin
             ,extGCD)
where

import qualified Data.Map.Strict as M
import qualified Data.Set as S

-- grab the common prefix (in reverse), and the two divergent suffices (forwards)
extractJoin :: Eq a => [a] -> [a] -> ([a],[a],[a])
extractJoin [] bs = ([],[],bs)
extractJoin as [] = ([],as,[])
extractJoin (a:as) (b:bs) | a == b = let (p,l,r) = extractJoin as bs
                                     in (a:p,l,r)
                          | otherwise = ([],a:as,b:bs)

type V2 = (Integer,Integer)

(.+) :: V2 -> V2 -> V2
(a,b) .+ (c,d) = (a+c,b+d)

(.-) :: V2 -> V2 -> V2
(a,b) .- (c,d) = (a-c,b-d)

parseCharArray :: String -> M.Map V2 Char
parseCharArray = M.fromList
               . concat
               . zipWith (\y l -> zipWith (\x c -> ((x,y),c)) [0..] l)
                 [0..]
               . lines

neighboursGrid :: V2 -> [V2]
neighboursGrid (x,y) = [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]

-- Dijkstra's algorithm for point-to-any shortest paths on a weighted graph
-- We lazily produce a list of distances from the start, in order of increasing distance.
dijkstra :: Ord a => (a -> [(Int,a)]) -> a -> [(Int,a)]
dijkstra children start = go (M.singleton start 0) S.empty (S.singleton (0,start))
-- Use a set as a poor-mans priority queue
  where go dist seen pq = case S.minView pq of
                            Nothing -> []
                            Just ((d,s),pq') ->
                              let new = filter (not . flip S.member seen . snd) $ map (\(dd,ch) -> (d+dd,ch)) $ children s
                                  newPQ = foldr S.insert pq' new
                                  newDist = foldr (\(d',a) -> M.insertWith min a d') dist new
                              in if S.member s seen
                                 then go dist seen pq'
                                 else (d,s) : go newDist (S.insert s seen) newPQ

-- extGCD a b = (d,m,n) where d = gcd a b = m*a+n*b
extGCD :: Integer -> Integer -> (Integer,Integer,Integer)
extGCD a b = go a b 1 0 0 1
  where go r0 0 s0 _ t0 _ = (r0,s0,t0)
        go r0 r1 s0 s1 t0 t1 = let (q,r) = quotRem r0 r1
                                   s = s0 - q * s1
                                   t = t0 - q * t1
                               in go r1 r s1 s t1 t
