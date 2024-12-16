{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Bifunctor (Bifunctor (second), first)
import Data.Foldable (minimumBy)
import Data.Map qualified as M
import Data.Ord (comparing)
import Data.Set qualified as S
import Data.Tuple (swap)
import Numeric.Natural (Natural)
import Utils (Grid (Grid, cts), parseGrid)

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

-- skew heap, ordered by 'a', with extra data 'b' just carried around
data Heap a b = Nil | Node a b (Heap a b) (Heap a b)

merge :: (Ord a) => Heap a b -> Heap a b -> Heap a b
merge Nil h = h
merge h Nil = h
merge h1@(Node n1 _ _ _) h2@(Node n2 _ _ _) =
  let (Node a b l r, h) = if n1 < n2 then (h1, h2) else (h2, h1)
   in Node a b (merge r h) l

singleton :: a -> b -> Heap a b
singleton a b = Node a b Nil Nil

viewMin :: (Ord a) => Heap a b -> Maybe (a, b, Heap a b)
viewMin = \case
  Nil -> Nothing
  Node a b l r -> Just (a, b, merge l r)

shortestPathLengths :: (Ord s) => (s -> [(Natural, s)]) -> s -> [(Natural, s)]
shortestPathLengths nbd start = go S.empty (singleton 0 start)
  where
    go seen h = case viewMin h of
      Nothing -> []
      Just (c, s, h')
        | S.member s seen -> go seen h'
        | otherwise ->
            (c, s)
              : go
                (S.insert s seen)
                ( foldr (merge . uncurry singleton) h' $
                    filter (\(_, s') -> S.notMember s' seen) $
                      map (first (+ c)) $
                        nbd s
                )

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
