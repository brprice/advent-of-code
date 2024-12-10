{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Char (digitToInt)
import Data.Map qualified as M
import Data.Maybe (mapMaybe)
import Data.Set qualified as S
import Utils (Grid (Grid, cts), parseGrid)

getData :: IO String
getData = readFile "../data/day10"

parse :: String -> Grid Int
parse = parseGrid (pure . digitToInt)

nbdCoords :: (Int, Int) -> [(Int, Int)]
nbdCoords (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

dp :: (((Int, Int), Int, [(Int, a)]) -> a) -> Grid Int -> Grid (Int, a)
dp f g@Grid {cts = m} =
  let cts = M.mapWithKey (\k v -> (v, f (k, v, mapMaybe (cts M.!?) $ nbdCoords k))) m
   in g {cts}

trailHeads :: Grid (Int, a) -> [a]
trailHeads = map snd . filter ((== 0) . fst) . M.elems . cts

part1 :: Grid Int -> Int
part1 = sum . map S.size . trailHeads . dp f
  where
    f = \case
      (p, 9, _) -> S.singleton p
      (_, n, nbd) -> S.unions $ map snd $ filter ((== n + 1) . fst) nbd

part2 :: Grid Int -> Int
part2 = sum . trailHeads . dp f
  where
    f = \case
      (p, 9, _) -> 1
      (_, n, nbd) -> sum $ map snd $ filter ((== n + 1) . fst) nbd

main :: IO ()
main = do
  xs <- parse <$> getData
  putStrLn "Part 1"
  print $ part1 xs
  putStrLn "Part 2"
  print $ part2 xs
