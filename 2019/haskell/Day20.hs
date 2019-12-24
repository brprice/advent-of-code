{-# LANGUAGE TupleSections #-}
module Main where

import Data.Char (isUpper)
import Data.Maybe (catMaybes, listToMaybe, mapMaybe)
import qualified Data.Map.Strict as M

import Utils

data Neighbour = SameLevel V2 -- move to this square in the same maze
               | Inner V2 -- move to this square in a smaller recursive maze
               | Outer V2 -- move to this square in a larger recursive maze

nonRecNeigh :: Neighbour -> V2
nonRecNeigh (SameLevel n) = n
nonRecNeigh (Inner n) = n
nonRecNeigh (Outer n) = n

recNeigh :: Integer -> Neighbour -> Maybe (Integer,V2)
recNeigh l (SameLevel n) = Just (l,n)
recNeigh l (Inner n) = Just (l+1,n)
recNeigh 0 (Outer _) = Nothing
recNeigh l (Outer n) = Just (l-1,n)

parseMaze :: M.Map V2 Char -> (M.Map V2 [Neighbour], M.Map String [V2])
parseMaze m = let corridors' = M.filter (=='.') m
                  corridors = flip M.mapWithKey corridors'
                            $ \p _ -> map SameLevel
                                    $ filter (flip M.member corridors')
                                    $ neighboursGrid p
                  labelChars = M.filter isUpper m
                  labelDirs = [(1,0),(0,1)]
                  getLabel (p1, l1) = listToMaybe $ catMaybes
                    [(\l2 -> ([l1,l2],p3)) <$> M.lookup p2 labelChars
                    | d <- labelDirs, let p2 = p1 .+ d, p3 <- [p2 .+ d, p1 .- d], M.member p3 corridors']
                  labelPos = M.fromListWith (++) $ map (\(l,p) -> (l,[p])) $ mapMaybe (getLabel) $ M.toList labelChars
                  (maxX,maxY) = foldr (\(x,y) (mx,my) -> (max x mx, max y my)) (0,0) $ M.keys m
                  portalType (x,y) = if x < 5 || x > maxX-5 || y < 5 || y > maxY-5
                                     then Inner (x,y) -- NB: (x,y) is location of target
                                     else Outer (x,y) -- so this is the other way that you may expect
                  portals = M.fromListWith (++)
                          $ flip concatMap (M.toList labelPos)
                          $ \(_,ps) -> case ps of
                                         [_] -> [] -- start or end portal
                                         [p,q] -> [(p,[portalType q]),(q,[portalType p])]
                                         _ -> error "malformed input data"
              in (M.unionWith (++) portals corridors, labelPos)

main :: IO ()
main = do dat <- readFile "../data/day20"
          let (maze, portals) = parseMaze $ parseCharArray dat
          let [start] = portals M.! "AA"
          let [end] = portals M.! "ZZ"
          let distsa = dijkstra (\p -> map (\n -> (1,nonRecNeigh n)) $ maze M.! p) start
          let parta = fst $ head $ filter ((==end).snd) distsa
          putStr "part a: "
          print parta

          let distsb = dijkstra (\(l,p) -> mapMaybe (\n -> (1,) <$> recNeigh l n) $ maze M.! p) (0,start)
          let partb = fst $ head $ filter ((==(0,end)).snd) distsb
          putStr "part b: "
          print partb
