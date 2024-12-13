{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Data.List (mapAccumL)
import Data.Map.Strict qualified as M
import Data.Maybe (isJust)
import Data.Monoid (Dual (Dual, getDual), Endo (Endo, appEndo), Sum (Sum))
import Data.Set qualified as S
import Utils (Grid (cts, height, width), parseGrid)

getData :: IO String
getData = readFile "../data/day12"

parse :: String -> Grid Char
parse = parseGrid Just

class (Monoid m) => CommutativeMonoid m

-- commutative-monoidal decorated union find
-- NB: I suspect this is pretty inefficient (in particular, part 1 was much slower than I expected)
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
  Parent b -> let (s, m, r, UF u') = find b uf in (s, m, r, UF $ M.insert a (Parent r) u')

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

part1 :: Grid Char -> Int
part1 g =
  let add x y =
        let hxy = (x, y)
            hf = g.cts M.! hxy
            uxy = (x, y - 1)
            uf = g.cts M.!? uxy
            lxy = (x - 1, y)
            lf = g.cts M.!? lxy
            rxy = (x + 1, y)
            rf = g.cts M.!? rxy
            dxy = (x, y + 1)
            df = g.cts M.!? dxy
            perim = length $ filter (/= Just hf) [uf, lf, rf, df]
            addu = if uf == Just hf then union hxy uxy else id
            addl = if lf == Just hf then union hxy lxy else id
         in Dual $ Endo $ addl . addu . insert (Sum perim) (x, y)
      coords = [(x, y) | x <- [0 .. g.width - 1], y <- [0 .. g.height - 1]]
      uf = appEndo (getDual $ mconcat [add x y | (x, y) <- coords]) empty
      roots =
        S.fromList $
          snd $
            mapAccumL
              (\uf' xy -> let (area, Sum perim, root, uf'') = find xy uf' in (uf'', (root, area * perim)))
              uf
              coords
   in sum $ map snd $ S.toList roots

main :: IO ()
main = do
  xs <- parse <$> getData
  putStrLn "Part 1"
  print $ part1 xs
