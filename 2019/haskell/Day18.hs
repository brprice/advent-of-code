{-# LANGUAGE FlexibleContexts, TupleSections #-}

module Main where

import Utils(extractJoin)

import Data.Char (isUpper, toLower)
import qualified Data.Map as M
import qualified Data.Set as S

-- represent a forest as edges towards the roots
type Forest a = M.Map a a
type V2 = (Integer,Integer)
type Key = Char
type Door = Char

-- We treat the entrance '@' as a key here
parseMaze :: String -> (S.Set V2, M.Map Key V2, M.Map V2 Door)
parseMaze s = let halls = filter (notWall.snd) $ concat $ zipWith (\y l -> zipWith (\x c -> ((x,y),c)) [0..] l) [0..] $ lines s
                  keys' = filter (isKey.snd) halls
                  doors' = filter (isDoor.snd) halls
                  keys = M.fromList $ map swap keys'
                  -- we treat each key as a door also, as it is pointless going past a key on the way to pick
                  -- up a different one!
                  doors = M.fromList $ doors' ++ keys'
              in (S.fromList $ map fst halls, keys, doors)
  where notWall = (/=) '#'
        isKey c = c/='.' && not (isUpper c)
        isDoor = isUpper
        swap (x,y) = (y,x)

neighbours :: V2 -> [V2]
neighbours (x,y) = [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]

buildForest :: S.Set V2 -> [V2] -> Forest V2
buildForest = go
  where go halls roots = go' halls (S.fromList roots) M.empty
                             [(r,n) | r<-roots, n<-neighbours r, S.member r halls, S.member n halls]
        go' _ _ forest [] = forest
        go' halls seen forest ((p,c):pcs)
          | S.member c seen = error "parseMase: not a forest!"
          | otherwise = go' halls (S.insert c seen) (M.insert c p forest) $ map (c,) (filter (\c' -> p/=c' && S.member c' halls) (neighbours c)) ++ pcs

-- Find minimal distance between each pair of keys, and the keys one must collect before being able to reach each particular key
-- Assumes roots of forest are connected, with l1 norm
distsBlocks :: (Forest V2,M.Map Key V2,M.Map V2 Door) -> (M.Map (Key,Key) Int, M.Map Key [Key])
distsBlocks (forest,keys,doors) = let kpds = M.map pathDoors keys
                                      kds = M.map snd kpds
                                      dists = M.fromList [((k1,k2),getDist kpds k1 k2) | k1<-M.keys keys, k2<-M.keys keys]
                                  in (dists,kds)
  where pathDoors :: V2 -> ([V2],[Key])
        -- path to root (including start and root) & (the key corresponding to) any doors on that path
        pathDoors l = case M.lookup l forest of
                        Nothing -> ([l],[]) -- this is a root
                        Just p -> let (pp,pd) = pathDoors p
                                  in (l:pp,maybe id ((:).toLower) (M.lookup p doors) pd)
        getDist :: M.Map Key ([V2],a) -> Key -> Key -> Int
        getDist paths k1 k2 = let p1 = reverse $ fst $ paths M.! k1
                                  p2 = reverse $ fst $ paths M.! k2
                                  r1 = head p1
                                  r2 = head p2
                                  (_,suf1,suf2) = extractJoin p1 p2
                              in if r1 == r2
                                 then length suf1 + length suf2 -- same part of forest
                                 else length p1 - 1 + length p2 - 1 + fromIntegral (abs(fst r1 - fst r2) + abs(snd r1 - snd r2))

-- Find the minimal length of a path starting at 'start', visiting all the
-- given nodes (the keys of the map), in some order s.t. all of the dependencies
-- (values of the map) are visited before the node itself.
-- Implemeneted via graph searching.
minPathVisiting :: Ord a => M.Map (a,a) Int -> a -> M.Map a (S.Set a) -> Int
minPathVisiting dists start deps = fst $ head $ filter (S.null.snd.snd) $ dij next (start,M.keysSet deps)
  where next (s,v) = let nxt = filter (\n -> S.null $ S.intersection v (deps M.! n)) $ S.elems v
                     in map (\n -> (dists M.! (s,n),(n,S.delete n v))) nxt

-- Dijkstra's algorithm for point-to-any shortest paths on a weighted graph
-- We lazily produce a list of distances from the start, in order of increasing distance.
dij :: Ord a => (a -> [(Int,a)]) -> a -> [(Int,a)]
dij children start = go (M.singleton start 0) S.empty (M.singleton 0 [start])
-- Use a map as a poor-mans priority queue
  where go dist seen pq = case M.minViewWithKey pq of
                            Nothing -> []
                            Just ((_,[]),pqRest) -> go dist seen pqRest
                            Just ((d,s:ss),pqRest) ->
                              let pq' = M.insert d ss pqRest
                                  new = filter (not . flip S.member seen . snd) $ map (\(dd,ch) -> (d+dd,ch)) $ children s
                                  newPQ = foldr (\(d',s') -> M.insertWith (++) d' [s']) pq' new
                                  newDist = foldr (\(d',a) -> M.insertWith min a d') dist new
                              in if S.member s seen
                                 then go dist seen pq'
                                 else (d,s) : go newDist (S.insert s seen) newPQ


main :: IO ()
main = do dat <- readFile "../data/day18"
          let (halls,keys,doors) = parseMaze dat
              (x,y) = keys M.! '@'
              roots = (x,y) : [(x+i,y+j) | i<-[-1,1],j<-[-1,1]]
              -- break the maze into 4 trees, one per quadrant,
              -- plus one trivial one at the entry (for measuring distance)
              halls' = halls S.\\ S.fromList (neighbours (x,y))
              forest = buildForest halls' roots
              -- add keys so part b can measure distance from them
              quadrantRoots = "1234"
              keys' = foldr (uncurry M.insert) keys $ zip quadrantRoots (tail roots)
              (dists,blocks) = distsBlocks (forest,keys',doors)
              blocks' = M.map S.fromList $ M.filterWithKey (\k _ -> k`notElem` '@' : quadrantRoots) blocks

          putStr "part a: "
          print $ minPathVisiting dists '@' blocks'
