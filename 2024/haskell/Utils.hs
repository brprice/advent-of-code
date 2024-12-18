{-# LANGUAGE LambdaCase #-}

module Utils
  ( Grid (..),
    parseGrid,
    invertMap,
    shortestPathLengths,
    CommutativeMonoid,
    UnionFind,
    empty,
    insert,
    find,
    union,
  )
where

import Data.Bifunctor (first)
import Data.Map qualified as M
import Data.Maybe (mapMaybe)
import Data.Monoid (Sum)
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

class (Monoid m) => CommutativeMonoid m

-- commutative-monoidal decorated union find
-- NB: I suspect this is pretty inefficient (in particular, Day 12 part 1 was much slower than I expected)
data UnionFind' m a = Root !Int !m | Parent !a
  deriving (Show)

newtype UnionFind m a = UF (M.Map a (UnionFind' m a))
  deriving (Show)

empty :: UnionFind m a
empty = UF M.empty

insert :: (Ord a) => m -> a -> UnionFind m a -> UnionFind m a
insert m a (UF u) = UF $ M.insertWith (error "key already exists") a (Root 1 m) u

-- find also "mutates" the structure, by "compressing paths"
-- It returns (size of set,decoration-of-set,representative-of-set) as well as mutated structure
find :: (Ord a) => a -> UnionFind m a -> (Int, m, a, UnionFind m a)
find a uf@(UF u) = case u M.! a of
  Root s m -> (s, m, a, uf)
  Parent b -> case u M.! b of
    Root s m -> (s, m, b, uf)
    Parent c -> find c $ UF $ M.insert a (Parent c) u

union :: (Ord a, CommutativeMonoid m) => a -> a -> UnionFind m a -> UnionFind m a
union a b u0 =
  let (sa, ma, ra, u1) = find a u0
      (sb, mb, rb, u2@(UF u2')) = find b u1
      (ra', rb') =
        if sa < sb
          then (Parent rb, Root (sa + sb) (ma <> mb))
          else (Root (sa + sb) (ma <> mb), Parent ra)
   in if ra == rb then u2 else UF $ M.insert ra ra' $ M.insert rb rb' u2'

instance CommutativeMonoid (Sum Int)
