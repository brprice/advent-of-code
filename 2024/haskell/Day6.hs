{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Either (partitionEithers)
import Data.Maybe (mapMaybe)
import Data.Set qualified as S

getData :: IO String
getData = readFile "../data/day6"

data Dir = L | U | R | D

data Guard = G {pos :: (Int, Int), dir :: Dir}

data Maze = M
  { width, height :: Int,
    obstr :: S.Set (Int, Int),
    guard :: Guard
  }

parse :: String -> Maze
parse s =
  let ls = lines s
      l' y =
        mapMaybe
          ( \case
              (_, '.') -> Nothing
              (x, '#') -> Just (Left (x, y))
              (x, '<') -> Just (Right ((x, y), L))
              (x, '^') -> Just (Right ((x, y), U))
              (x, '>') -> Just (Right ((x, y), R))
              (x, 'v') -> Just (Right ((x, y), D))
          )
          . zip [0 ..]
      ls' = partitionEithers . concatMap (uncurry l') . zip [0 ..]
      (obstr, [guard]) = ls' ls
   in M
        { width = length (head ls),
          height = length ls,
          obstr = S.fromList obstr,
          guard = uncurry G guard
        }

iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f x =
  x : case f x of
    Nothing -> []
    Just y -> iterateMaybe f y

step1 :: (Int, Int) -> Dir -> (Int, Int)
step1 (x, y) = \case
  L -> (x - 1, y)
  U -> (x, y - 1)
  R -> (x + 1, y)
  D -> (x, y + 1)

turnRight :: Guard -> Guard
turnRight (G p d) = G p $ case d of
  L -> U
  U -> R
  R -> D
  D -> L

step :: Maze -> Maybe Maze
step m =
  let n@(x, y) = step1 (pos $ guard m) (dir $ guard m)
   in if x < 0 || y < 0 || x >= width m || y >= height m
        then Nothing
        else
          if S.member n $ obstr m
            then Just m {guard = turnRight $ guard m}
            else Just m {guard = (guard m) {pos = n}}

part1 :: Maze -> Int
part1 m = length $ S.fromList $ map (pos . guard) $ iterateMaybe step m

main :: IO ()
main = do
  xs <- parse <$> getData
  putStrLn "Part 1"
  print $ part1 xs
