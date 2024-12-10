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

part1 :: Grid Int -> Int
part1 Grid {cts = m} =
  let nbd (x, y) = mapMaybe (flip M.lookup reachable) [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
      r k v =
        ( m M.! k,
          case v of
            9 -> S.singleton k
            n -> S.unions $ map snd $ filter ((== n + 1) . fst) $ nbd k
        )
      reachable = M.mapWithKey r m
      trailHeads = filter ((== 0) . fst) $ M.elems reachable
   in sum $ map (S.size . snd) trailHeads

main :: IO ()
main = do
  xs <- parse <$> getData
  putStrLn "Part 1"
  print $ part1 xs
