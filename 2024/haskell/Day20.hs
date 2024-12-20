{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Tuple (swap)
import Utils (Grid (Grid, cts), parseGrid, shortestPathLengths)
import Numeric.Natural (Natural)

getData :: IO String
getData = readFile "../data/day20"

data Maze = M
  { start :: (Int, Int),
    end :: (Int, Int),
    squares :: S.Set (Int, Int)
  }

parse :: String -> Maze
parse s =
  let parseCell = \case
        'S' -> Just $ Left $ Left ()
        'E' -> Just $ Left $ Right ()
        '.' -> Just $ Right ()
        _ -> Nothing
      Grid {cts} = parseGrid parseCell s
      (startEnd, _) = M.mapEither id cts
      (start, end) = M.mapEither id startEnd
   in M (fst $ M.findMin start) (fst $ M.findMin end) (M.keysSet cts)

range :: Int -> [(Int, Int)]
range range = concat [mirrors (x, y) | x <- [0 .. range], y <- [0 .. range - x]]
  where
    mirrors = \case
      (0, 0) -> []
      (0, y) -> [(0, y), (0, -y)]
      (x, 0) -> [(x, 0), (-x, 0)]
      (x, y) -> [(x, y), (-x, y), (x, -y), (-x, -y)]

cheatSave :: Int -> Natural -> Maze -> [((Int,Int),(Int,Int),Natural)]
cheatSave cheatLength saving m = filter (\(_, _, l) -> l <= noCheat - saving) cheats
  where
    nbd (x, y) = map (1,) $ filter (`S.member` m.squares) [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
    noCheatFromStart = M.fromList $ map swap $ shortestPathLengths nbd m.start
    noCheatToEnd = M.fromList $ map swap $ shortestPathLengths nbd m.end
    noCheat = noCheatToEnd M.! m.start
    cheatPos =
      concatMap
        ( \(x, y) ->
            map (((x, y),)) $
              filter (`S.member` m.squares) $
                map (\(dx, dy) -> (x + dx, y + dy)) $
                  range cheatLength
        )
        m.squares
    dist (x1 , y1) (x2,y2) = fromIntegral $ abs (x1 - x2) + abs (y1 - y2)
    cheats = map (\(s, e) -> (s, e, noCheatFromStart M.! s + dist s e + noCheatToEnd M.! e)) cheatPos

part1 :: Maze -> Int
part1 = length . cheatSave 2 100

part2 :: Maze -> Int
part2 = length . cheatSave 20 100

main :: IO ()
main = do
  xs <- parse <$> getData
  putStrLn "Part 1"
  print $ part1 xs
  putStrLn "Part 2"
  print $ part2 xs
