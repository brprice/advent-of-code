{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Utils
  ( Grid (Grid, cts, height, width),
    invertMap,
    parseGrid,
  )

getData :: IO String
getData = readFile "../data/day8"

parse :: String -> Grid Char
parse = parseGrid $ \case
  '.' -> Nothing
  c -> Just c

part1 :: Grid Char -> Int
part1 Grid {width, height, cts} =
  let csLoc = invertMap cts
      locs' (x1, y1) (x2, y2) =
        let dx = x2 - x1
            dy = y2 - y1
         in [(x1 - dx, y1 - dy), (x2 + dx, y2 + dy)]
      locs c =
        let poss = S.elems $ csLoc M.! c
         in concat [locs' p1 p2 | p1 <- poss, p2 <- poss, p1 /= p2]
      inBounds (x, y) = 0 <= x && x < width && 0 <= y && y < height
   in length $ S.fromList $ filter inBounds $ concatMap locs $ M.keys csLoc

part2 :: Grid Char -> Int
part2 Grid {width, height, cts} =
  let csLoc = invertMap cts
      locs' (x1, y1) (x2, y2) =
        let dx = x2 - x1
            dy = y2 - y1
            ms = map (\n -> (x1 - n*dx, y1 - n*dy)) [0..]
            ps = map (\n -> (x2 + n*dx, y2 + n*dy)) [0..]
         in (takeWhile inBounds ms) ++ (takeWhile inBounds ps)
      locs c =
        let poss = S.elems $ csLoc M.! c
         in concat [locs' p1 p2 | p1 <- poss, p2 <- poss, p1 /= p2]
      inBounds (x, y) = 0 <= x && x < width && 0 <= y && y < height
   in length $ S.fromList $ concatMap locs $ M.keys csLoc

main :: IO ()
main = do
  xs <- parse <$> getData
  putStrLn "Part 1"
  print $ part1 xs
  putStrLn "Part 2"
  print $ part2 xs
