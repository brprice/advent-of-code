{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Data.Map.Lazy qualified as ML
import Data.Map.Strict qualified as M
import Data.Maybe (mapMaybe)
import Data.Set qualified as S
import Data.Tuple (swap)
import Numeric.Natural (Natural)
import Utils (shortestPathLengths)

getData :: IO String
getData = readFile "../data/day21"

parse :: String -> [[Char]]
parse = lines

data Dir = U | D | L | R

data Layer = Layer {keys :: S.Set Char, move :: Char -> Dir -> Maybe Char}

numLayer :: Layer
numLayer =
  Layer
    { keys = S.fromList "0123456789A",
      move = \cases
        '7' R -> Just '8'
        '7' D -> Just '4'
        '8' L -> Just '7'
        '8' R -> Just '9'
        '8' D -> Just '5'
        '9' L -> Just '8'
        '9' D -> Just '6'
        '4' U -> Just '7'
        '4' R -> Just '5'
        '4' D -> Just '1'
        '5' U -> Just '8'
        '5' L -> Just '4'
        '5' R -> Just '6'
        '5' D -> Just '2'
        '6' U -> Just '9'
        '6' L -> Just '5'
        '6' D -> Just '3'
        '1' U -> Just '4'
        '1' R -> Just '2'
        '2' U -> Just '5'
        '2' L -> Just '1'
        '2' R -> Just '3'
        '2' D -> Just '0'
        '3' U -> Just '6'
        '3' L -> Just '2'
        '3' D -> Just 'A'
        '0' U -> Just '2'
        '0' R -> Just 'A'
        'A' U -> Just '3'
        'A' L -> Just '0'
        _ _ -> Nothing
    }

dirLayer :: Layer
dirLayer =
  Layer
    { keys = S.fromList "<^>vA",
      move = \cases
        '^' R -> Just 'A'
        '^' D -> Just 'v'
        'A' L -> Just '^'
        'A' D -> Just '>'
        '<' R -> Just 'v'
        'v' U -> Just '^'
        'v' L -> Just '<'
        'v' R -> Just '>'
        '>' U -> Just 'A'
        '>' L -> Just 'v'
        _ _ -> Nothing
    }

charToDir :: Char -> Dir
charToDir = \case
  '<' -> L
  '>' -> R
  '^' -> U
  'v' -> D

step :: [Char] -> Char -> Maybe [Char]
step [n] = \case
  'A' -> Nothing
  c -> (: []) <$> numLayer.move n (charToDir c)
step (d : cs) = \case
  'A' -> (d :) <$> step cs d
  c -> (: cs) <$> dirLayer.move d (charToDir c)

steps :: [Char] -> [(Natural, [Char])]
steps cs = mapMaybe (fmap (1,) . step cs) "^A<v>"

part1 :: [String] -> Natural
part1 = sum . map (\s -> read (init s) * part1' s)
  where
    part1' s =
      let waypoints = map (\c -> ['A', 'A', c]) s
          l f t = head $ filter ((== t) . snd) $ shortestPathLengths steps f
       in sum $ map ((+ 1) . fst) $ zipWith l ("AAA" : waypoints) waypoints

numLayer2 :: (S.Set (Int, Int), Char -> (Int, Int))
numLayer2 = (S.fromList $ map f "0123456789A", f)
  where
    f = \case
      '7' -> (0, 0)
      '8' -> (1, 0)
      '9' -> (2, 0)
      '4' -> (0, 1)
      '5' -> (1, 1)
      '6' -> (2, 1)
      '1' -> (0, 2)
      '2' -> (1, 2)
      '3' -> (2, 2)
      '0' -> (1, 3)
      'A' -> (2, 3)

dirLayer2 :: (S.Set (Int, Int), Char -> (Int, Int))
dirLayer2 = (S.fromList $ map f "^A<v>", f)
  where
    f = \case
      '^' -> (1, 0)
      'A' -> (2, 0)
      '<' -> (0, 1)
      'v' -> (1, 1)
      '>' -> (2, 1)

moves :: (S.Set (Int, Int), Char -> (Int, Int)) -> Char -> Char -> [String]
moves (layerPos, layerF) (layerF -> from@(sx, sy)) (layerF -> to@(tx, ty)) =
  go
    from
    to
    (if tx < sx then (-1, '<') else (1, '>'))
    (if ty < sy then (-1, '^') else (1, 'v'))
  where
    go s@(sx, sy) t@(tx, ty) h@(hd, hc) v@(vd, vc)
      | S.notMember s layerPos = []
      | sx == tx && sy == ty = [""]
      | sx == tx = [replicate (abs $ ty - sy) vc]
      | sy == ty = [replicate (abs $ tx - sx) hc]
      | otherwise = map (hc :) (go (sx + hd, sy) t h v) ++ map (vc :) (go (sx, sy + vd) t h v)

bestDirPads :: M.Map (Int, Char, Char) Natural
bestDirPads = ML.fromList $ base ++ concatMap induct [1 .. 25]
  where
    dirs = "^<v>"
    keys = "^A<v>"
    base = [((1, s, t), (+ 1) $ fromIntegral $ length $ head $ moves dirLayer2 s t) | s <- keys, t <- keys]
    induct n =
      let f s t = minimum $ map h $ moves dirLayer2 s t
          h ms = sum $ zipWith (\s t -> bestDirPads M.! (n, s, t)) ('A' : ms) (ms ++ "A")
       in [((n + 1, s, t), f s t) | s <- keys, t <- keys]

part2 :: [String] -> Natural
part2 = sum . map (\s -> read (init s) * part2' s)
  where
    moveCost ms = sum $ zipWith (\s t -> bestDirPads ML.! (25, s, t)) ('A' : ms) (ms ++ "A")
    part2' s =
      let numMoves = zipWith (moves numLayer2) ('A' : s) s
       in sum $ map (minimum . map moveCost) numMoves

main :: IO ()
main = do
  xs <- parse <$> getData
  putStrLn "Part 1"
  print $ part1 xs
  putStrLn "Part 2"
  print $ part2 xs
