{-# LANGUAGE LambdaCase #-}

module Utils
  ( Grid (..),
    parseGrid,
    invertMap,
    shortestPathLengths,
  )
where

import Data.Bifunctor (first)
import Data.Map qualified as M
import Data.Maybe (mapMaybe)
import Data.Set qualified as S
import Numeric.Natural (Natural)

data Grid a = Grid
  { width, height :: Int,
    -- coordinates are (0,0) is top-left
    -- and (width-1,height-1) is bottom-right
    cts :: M.Map (Int, Int) a
  }
  deriving (Show)

parseGrid :: (Char -> Maybe a) -> String -> Grid a
parseGrid f s =
  let ls = lines s
      l' y = mapMaybe (traverse f) . zip (map (,y) [0 ..])
      ls' = concatMap (uncurry l') . zip [0 ..]
      cts = ls' ls
   in Grid
        { width = length (head ls),
          height = length ls,
          cts = M.fromList cts
        }

invertMap :: (Ord k, Ord v) => M.Map k v -> M.Map v (S.Set k)
invertMap = M.fromListWith (<>) . map (\(k, v) -> (v, S.singleton k)) . M.toList

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
