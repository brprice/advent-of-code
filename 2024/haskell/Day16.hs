{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Bifunctor (first)
import Data.Map qualified as M
import Data.Set qualified as S
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

search :: Ord s => (s -> [(Natural, s)]) -> s -> (s -> Bool) -> Natural
search nbd start target = go S.empty (singleton 0 start)
  where
    go seen h = case viewMin h of
      Just (c, s, h')
        | target s -> c
        | otherwise -> go (S.insert s seen) $ foldr (merge . uncurry singleton) h'
            $ filter (\(_,s') -> S.notMember s' seen)
            $ map (first (+ c)) $ nbd s

part1 :: Maze -> Natural
part1 m =
  search
    ( \((x, y), d) ->
        (1000, ((x, y), cw d))
          : (1000, ((x, y), ccw d))
          : if S.member (fwd (x, y) d) (squares m)
            then [(1, (fwd (x, y) d, d))]
            else []
    )
    (start m)
    (`elem` [(end m, d) | d <- [N, E, S, W]])
  where
    cw = \case
      N -> E
      E -> S
      S -> W
      W -> N
    ccw = \case
      N -> W
      E -> N
      S -> E
      W -> S
    fwd (x, y) = \case
      N -> (x, y - 1)
      E -> (x + 1, y)
      S -> (x, y + 1)
      W -> (x - 1, y)

main :: IO ()
main = do
  xs <- parse <$> getData
  putStrLn "Part 1"
  print $ part1 xs
