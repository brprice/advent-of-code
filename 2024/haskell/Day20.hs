{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Tuple (swap)
import Utils (Grid (Grid, cts), parseGrid, shortestPathLengths)

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

part1 :: Maze -> Int
part1 m = length $ filter (\(_, _, l) -> l <= noCheat - 100) cheats
  where
    nbd (x, y) = map (1,) $ filter (`S.member` m.squares) [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
    noCheatFromStart = M.fromList $ map swap $ shortestPathLengths nbd m.start
    noCheatToEnd = M.fromList $ map swap $ shortestPathLengths nbd m.end
    noCheat = noCheatToEnd M.! m.start
    cheatPos =
      concatMap
        ( \(x, y) ->
            map (((x, y),) . snd) $
              filter
                (\(c, e) -> S.member e m.squares && S.notMember c m.squares)
                [ ((x + 1, y), (x + 2, y)),
                  ((x - 1, y), (x - 2, y)),
                  ((x, y + 1), (x, y + 2)),
                  ((x, y - 1), (x, y - 2))
                ]
        )
        m.squares
    cheats = map (\(s, e) -> (s, e, noCheatFromStart M.! s + 2 + noCheatToEnd M.! e)) cheatPos

main :: IO ()
main = do
  xs <- parse <$> getData
  putStrLn "Part 1"
  print $ part1 xs
