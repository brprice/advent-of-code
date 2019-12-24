{-# LANGUAGE FlexibleContexts, TupleSections #-}

module Main where

import Utils(dijkstra, extractJoin, neighboursGrid, parseCharArray)

import Data.Char (isUpper, toLower)
import qualified Data.Map as M
import qualified Data.Set as S

-- represent a forest as edges towards the roots
type Forest a = M.Map a a
type V2 = (Integer,Integer)
type Key = Char
type Door = Char

-- We treat the entrance '@' as a key here
parseMaze :: M.Map V2 Char -> (S.Set V2, M.Map Key V2, M.Map V2 Door)
parseMaze m = let halls = M.filter notWall m
                  keys' = M.filter isKey halls
                  doors' = M.filter isDoor halls
                  keys = M.fromList $ map swap $ M.toList keys'
                  -- we treat each key as a door also, as it is pointless going past a key on the way to pick
                  -- up a different one!
                  doors = M.union doors' keys'
              in (M.keysSet halls, keys, doors)
  where notWall = (/=) '#'
        isKey c = c/='.' && not (isUpper c)
        isDoor = isUpper
        swap (x,y) = (y,x)

buildForest :: S.Set V2 -> [V2] -> Forest V2
buildForest = go
  where go halls roots = go' halls (S.fromList roots) M.empty
                             [(r,n) | r<-roots, n<-neighboursGrid r, S.member r halls, S.member n halls]
        go' _ _ forest [] = forest
        go' halls seen forest ((p,c):pcs)
          | S.member c seen = error "parseMase: not a forest!"
          | otherwise = go' halls (S.insert c seen) (M.insert c p forest) $ map (c,) (filter (\c' -> p/=c' && S.member c' halls) (neighboursGrid c)) ++ pcs

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
minPathVisiting :: Ord a => M.Map (a,a) Int -> a -> M.Map a (S.Set a) -> Int
minPathVisiting dists start deps = minMultiPathVisiting dists deps [(start,M.keysSet deps)]

-- Find the minimal total length of multiple paths starting at particular elements
-- and visiting a set of nodes each, whilst respecting the order given by the dependency
-- map. The ordering is respected across different paths.
-- Implemeneted via graph searching.
minMultiPathVisiting :: Ord a => M.Map (a,a) Int -> M.Map a (S.Set a) -> [(a, S.Set a)] -> Int
minMultiPathVisiting dists deps = fst . head . filter (\(_,s) -> all (S.null . snd) s)
                                . dijkstra (\s -> modify1 (step1 $ S.unions $ map snd s) s)
  where -- modify1 non-deterministically applies f to one of the input elts,
        -- returning the by-product, and updating the element
        modify1 :: (a -> [(b,a)]) -> [a] -> [(b,[a])]
        modify1 _ [] = error "modify1 on empty list"
        modify1 f [x] = map (\(b,y) -> (b,[y])) $ f x
        modify1 f (x:xs) = let bxs' = f x
                           in map (\(b,x') -> (b,x':xs)) bxs' ++ map (\(c,ys) -> (c,x:ys)) (modify1 f xs)
        step1 allToVisit (start,toVisit) = let tovi = S.toList toVisit
                                               next = filter (\n -> S.null $ S.intersection allToVisit (deps M.! n)) tovi
                                           in map (\n -> (dists M.! (start,n), (n,S.delete n toVisit))) next

main :: IO ()
main = do dat <- readFile "../data/day18"
          let (halls,keys,doors) = parseMaze $ parseCharArray dat
              (x,y) = keys M.! '@'
              roots = (x,y) : [(x+i,y+j) | i<-[-1,1],j<-[-1,1]]
              -- break the maze into 4 trees, one per quadrant,
              -- plus one trivial one at the entry (for measuring distance)
              halls' = halls S.\\ S.fromList (neighboursGrid (x,y))
              forest = buildForest halls' roots
              -- add keys so part b can measure distance from them
              quadrantRoots = "1234"
              keys' = foldr (uncurry M.insert) keys $ zip quadrantRoots (tail roots)
              (dists,blocks) = distsBlocks (forest,keys',doors)
              blocks' = M.map S.fromList $ M.filterWithKey (\k _ -> k`notElem` '@' : quadrantRoots) blocks

          putStr "part a: "
          print $ minPathVisiting dists '@' blocks'

          -- split keys into the 4 trees
          let inTree r k = dists M.! (r,k) == minimum (map (\r' -> dists M.! (r',k)) "1234")
          let vaults = map (\r -> (r,S.filter (\k -> (k `notElem` '@':quadrantRoots) && inTree r k) $ M.keysSet keys'))
                           quadrantRoots
          putStr "part b: "
          print $ minMultiPathVisiting dists blocks' vaults
