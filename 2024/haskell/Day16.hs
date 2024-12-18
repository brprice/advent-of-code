{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Bifunctor (Bifunctor (second), first)
import Data.Foldable (minimumBy)
import Data.Map qualified as M
import Data.Ord (comparing)
import Data.Set qualified as S
import Data.Tuple (swap)
import Numeric.Natural (Natural)
import Utils (Grid (Grid, cts), parseGrid, shortestPathLengths)

getData :: IO String
getData = readFile "../data/day16"

data Dir = N | E | S | W
  deriving (Eq, Ord)

data Maze = M
  { start :: ((Int, Int), Dir),
    end :: (Int, Int),
    squares :: S.Set (Int, Int)
  }

parse :: String -> Maze
parse s =
  let parseCell = \case
        'S' -> Just $ Left $ Left E
        'E' -> Just $ Left $ Right ()
        '.' -> Just $ Right ()
        _ -> Nothing
      Grid {cts} = parseGrid parseCell s
      (startEnd, _) = M.mapEither id cts
      (start, end) = M.mapEither id startEnd
   in M (M.findMin start) (fst $ M.findMin end) (M.keysSet cts)

cw :: Dir -> Dir
cw = \case
  N -> E
  E -> S
  S -> W
  W -> N

ccw :: Dir -> Dir
ccw = \case
  N -> W
  E -> N
  S -> E
  W -> S

fwd :: (Int, Int) -> Dir -> (Int, Int)
fwd (x, y) = \case
  N -> (x, y - 1)
  E -> (x + 1, y)
  S -> (x, y + 1)
  W -> (x - 1, y)

pathLengths :: Maze -> [(Natural, ((Int, Int), Dir))]
pathLengths m =
  shortestPathLengths
    ( \((x, y), d) ->
        (1000, ((x, y), cw d))
          : (1000, ((x, y), ccw d))
          : if S.member (fwd (x, y) d) (squares m)
            then [(1, (fwd (x, y) d, d))]
            else []
    )
    (start m)

part1 :: Maze -> Natural
part1 m = fst $ head $ filter (\(_, (p, _)) -> p == end m) $ pathLengths m

part2 :: Maze -> Int
part2 m = S.size $ go S.empty (S.singleton e)
  where
    pls = M.fromList $ map (swap . first fromIntegral) $ pathLengths m
    e = minimumBy (comparing $ flip M.lookup pls) $ map (end m,) [N, E, S, W]
    go done todo = case S.maxView todo of
      Nothing -> done
      Just (s, todo') ->
        let c = pls M.! s
            s' =
              map snd $
                filter
                  (\(c', s') -> Just c' == pls M.!? s')
                  [ (c - 1000, second ccw s),
                    (c - 1000, second cw s),
                    (c - 1, first (flip fwd $ cw $ cw $ snd s) s)
                  ]
         in go (S.insert (fst s) done) (foldr S.insert todo' s')

main :: IO ()
main = do
  xs <- parse <$> getData
  putStrLn "Part 1"
  print $ part1 xs
  putStrLn "Part 2"
  print $ part2 xs
