module Main where

import Data.Set qualified as S
import Numeric.Natural (Natural)
import Utils (shortestPathLengths)

getData :: IO String
getData = readFile "../data/day18"

parse :: String -> [(Int, Int)]
parse = map p . lines
  where
    p s = case break (== ',') s of
      (l, ',' : r) -> (read l, read r)

part1 :: [(Int, Int)] -> Natural
part1 xys = fst $ head $ filter ((== (70, 70)) . snd) $ shortestPathLengths nbd (0, 0)
  where
    bad = S.fromList $ take 1024 xys
    nbd (x, y) = [(1, (a, b)) | (dx, dy) <- [(-1, 0), (1, 0), (0, -1), (0, 1)], let a = x + dx, 0 <= a, a <= 70, let b = y + dy, 0 <= b, b <= 70, S.notMember (a, b) bad]

main :: IO ()
main = do
  xs <- parse <$> getData
  putStrLn "Part 1"
  print $ part1 xs
