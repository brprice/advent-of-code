{-# LANGUAGE LambdaCase #-}

module Main where

import Data.List (foldl')
import Data.Map.Strict qualified as M
import Data.Monoid (Sum (Sum, getSum))
import Utils (Grid (Grid, cts), parseGrid)

getData :: IO String
getData = readFile "../data/day15"

data Obstr = Box | Wall
  deriving (Eq)

data Warehouse = W {robot :: !(Int, Int), obstructions :: M.Map (Int, Int) Obstr}

(+.) :: (Int, Int) -> (Int, Int) -> (Int, Int)
(a, b) +. (c, d) = (a + c, b + d)

parse :: String -> (Warehouse, [(Int, Int)])
parse s =
  let ls = lines s
      (warehouse, "" : dirs) = break (== "") ls
      f = \case
        '.' -> Nothing
        'O' -> Just $ Right Box
        '#' -> Just $ Right Wall
        '@' -> Just $ Left ()
      Grid {cts} = parseGrid f $ unlines warehouse
      (robot, obstacles) = M.mapEither id cts
      g = \case
        '^' -> (0, -1)
        '>' -> (1, 0)
        'v' -> (0, 1)
        '<' -> (-1, 0)
   in (W (fst $ M.findMin robot) obstacles, map g $ concat dirs)

step :: Warehouse -> (Int, Int) -> Warehouse
step w@W {robot, obstructions} dir =
  let next = robot +. dir
      endObstrs =
        head $
          filter ((/= Just Box) . snd) $
            map (\c -> (c, obstructions M.!? c)) $
              iterate (+. dir) next
   in case endObstrs of
        (c, Nothing)
          | c == next -> W next obstructions
          | otherwise -> W next (M.insert c Box $ M.delete next obstructions)
        (_, Just Wall) -> w

part1 :: (Warehouse, [(Int, Int)]) -> Int
part1 (w, dirs) = getSum $ foldMap gps $ M.toList $ obstructions $ foldl' step w dirs
  where
    gps =
      Sum . \case
        ((x, y), Box) -> 100 * y + x
        (_, Wall) -> 0

main :: IO ()
main = do
  xs <- parse <$> getData
  putStrLn "Part 1"
  print $ part1 xs
