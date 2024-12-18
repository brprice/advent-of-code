module Main where

import Data.List (mapAccumL)
import Data.Monoid (Endo (Endo, appEndo))
import Data.Set qualified as S
import Numeric.Natural (Natural)
import Utils (empty, find, insert, shortestPathLengths, union)

getData :: IO String
getData = readFile "../data/day18"

parse :: String -> [(Int, Int)]
parse = map p . lines
  where
    p s = case break (== ',') s of
      (l, ',' : r) -> (read l, read r)

nbd :: (Int, Int) -> [(Int, Int)]
nbd (x, y) = [(a, b) | (dx, dy) <- [(-1, 0), (1, 0), (0, -1), (0, 1)], let a = x + dx, 0 <= a, a <= 70, let b = y + dy, 0 <= b, b <= 70]

part1 :: [(Int, Int)] -> Natural
part1 xys = fst $ head $ filter ((== (70, 70)) . snd) $ shortestPathLengths nbd' (0, 0)
  where
    bad = S.fromList $ take 1024 xys
    nbd' = map (1,) . filter (`S.notMember` bad) . nbd

part2 :: [(Int, Int)] -> (Int, Int)
part2 xys = fst $ head $ filter (isRoute . snd) seqConnected
  where
    (allBad : bads) = scanr S.insert S.empty $ reverse xys
    steps = zip (reverse xys) bads
    neverHit = [(x, y) | x <- [0 .. 70], y <- [0 .. 70], S.notMember (x, y) allBad]
    unconnected = flip appEndo empty $ foldMap (Endo . insert ()) neverHit
    initConnected =
      flip appEndo unconnected $
        mconcat
          [Endo $ union p q | p <- neverHit, q <- nbd p, S.notMember q allBad]
    getRoot p uf = case find p uf of (_, _, r, _) -> r
    isRoute uf = getRoot (0, 0) uf == getRoot (70, 70) uf
    seqConnected =
      snd $
        mapAccumL
          ( \uf (p, bad) ->
              let uf' =
                    flip appEndo (insert () p uf) $
                      mconcat
                        [Endo $ union p q | q <- nbd p, S.notMember q bad]
               in (uf', (p, uf'))
          )
          initConnected
          steps

main :: IO ()
main = do
  xs <- parse <$> getData
  putStrLn "Part 1"
  print $ part1 xs
  putStrLn "Part 2"
  print $ part2 xs
