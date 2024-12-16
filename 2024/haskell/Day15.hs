{-# LANGUAGE LambdaCase #-}

module Main where

import Data.List (foldl')
import Data.Map.Strict qualified as M
import Data.Monoid (Sum (Sum, getSum))
import Data.Set qualified as S
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

data Box2 = L | R

data Obstr2 = Box2 Box2 | Wall2

data Warehouse2 = W2 {robot2 :: !(Int, Int), obstructions2 :: M.Map (Int, Int) Obstr2}

step2 :: Warehouse2 -> (Int, Int) -> Warehouse2
step2 w@W2 {robot2, obstructions2} dir =
  let next = robot2 +. dir
      bothHalves p = \case
        L -> [(p, L), (p +. (1, 0), R)]
        R -> [(p, R), (p +. (-1, 0), L)]
      -- one-step "what boxes are pushed on" (Nothing if a wall is)
      sb :: [((Int, Int), Box2)] -> Maybe [((Int, Int), Box2)]
      sb =
        fmap concat
          . traverse
            ( \(p, _) -> case obstructions2 M.!? (p +. dir) of
                Nothing -> Just []
                Just Wall2 -> Nothing
                Just (Box2 b) -> Just $ filter ((/= p) . fst) $ bothHalves (p +. dir) b
            )
      -- multi-step unfolding of sb
      affected :: [((Int, Int), Box2)] -> Maybe [((Int, Int), Box2)]
      affected = \case
        [] -> Just []
        b -> fmap (b ++) $ affected =<< sb b
   in case obstructions2 M.!? next of
        Nothing -> w {robot2 = next}
        Just Wall2 -> w
        Just (Box2 b) -> case affected $ bothHalves next b of
          Nothing -> w
          Just bs ->
            W2 next $
              M.union (M.fromList $ map (\(p, b) -> (p +. dir, Box2 b)) bs) $
                M.withoutKeys obstructions2 (S.fromList $ map fst bs)

part2 :: (Warehouse, [(Int, Int)]) -> Int
part2 (W {robot = (rx, ry), obstructions}, dirs) =
  let w2 =
        W2 (rx * 2, ry) $
          M.fromList $
            M.foldMapWithKey
              ( \(x, y) -> \case
                  Box -> [((2 * x, y), Box2 L), ((2 * x + 1, y), Box2 R)]
                  Wall -> [((2 * x, y), Wall2), ((2 * x + 1, y), Wall2)]
              )
              obstructions
   in getSum $ foldMap gps $ M.toList $ obstructions2 $ foldl' step2 w2 dirs
  where
    gps =
      Sum . \case
        ((x, y), Box2 L) -> 100 * y + x
        (_, Box2 R) -> 0
        (_, Wall2) -> 0

main :: IO ()
main = do
  xs <- parse <$> getData
  putStrLn "Part 1"
  print $ part1 xs
  putStrLn "Part 2"
  print $ part2 xs
