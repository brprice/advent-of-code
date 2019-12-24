{-# LANGUAGE TupleSections #-}
module Main where

import Data.Char (isUpper)
import Data.Maybe (catMaybes, listToMaybe, mapMaybe)
import qualified Data.Map.Strict as M

import Utils

parseMaze :: M.Map V2 Char -> (M.Map V2 [V2], M.Map String [V2])
parseMaze m = let corridors' = M.filter (=='.') m
                  corridors = flip M.mapWithKey corridors'
                            $ \p _ -> filter (flip M.member corridors') $ neighboursGrid p
                  labelChars = M.filter isUpper m
                  labelDirs = [(1,0),(0,1)]
                  getLabel (p1, l1) = listToMaybe $ catMaybes
                    [(\l2 -> ([l1,l2],p3)) <$> M.lookup p2 labelChars
                    | d <- labelDirs, let p2 = p1 .+ d, p3 <- [p2 .+ d, p1 .- d], M.member p3 corridors']
                  labelPos = M.fromListWith (++) $ map (\(l,p) -> (l,[p])) $ mapMaybe (getLabel) $ M.toList labelChars
                  portals = M.fromListWith (++)
                          $ flip concatMap (M.toList labelPos)
                          $ \(_,ps) -> case ps of
                                         [_] -> [] -- start or end portal
                                         [p,q] -> [(p,[q]),(q,[p])]
                                         _ -> error "malformed input data"
              in (M.unionWith (++) portals corridors, labelPos)

main :: IO ()
main = do dat <- readFile "../data/day20"
          let (maze, portals) = parseMaze $ parseCharArray dat
          let [start] = portals M.! "AA"
          let [end] = portals M.! "ZZ"
          let dists = dijkstra (\p -> map (1,) $ maze M.! p) start
          let parta = fst $ head $ filter ((==end).snd) dists
          putStr "part a: "
          print parta
