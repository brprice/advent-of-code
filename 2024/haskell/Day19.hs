{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Data.Map.Lazy qualified as M
import Data.Set qualified as S

getData :: IO String
getData = readFile "../data/day19"

parse :: String -> ([String], [String])
parse s = case lines s of
  (ts : "" : ps) -> (map init (words $ ts ++","), ps)

part1 :: ([String], [String]) -> Int
part1 (ts, ps) = length $ filter (expressible $ map (\s -> (length s, s)) ts) ps
  where
    expressible ts = \case
      "" -> True
      p -> any (\(l, t) -> let (x, y) = splitAt l p in x == t && expressible ts y) ts

type OpenRec a = a -> a

data MemoTrie' c a = MT' {here :: a, there :: M.Map c (MemoTrie' c a)}

data MemoTrie c a = MT {opts :: [c], mt :: MemoTrie' c a}

lookupMT :: (Ord c) => MemoTrie c a -> [c] -> a
lookupMT = lookupMT' . mt
  where
    lookupMT' t = \case
      [] -> t.here
      c : cs -> lookupMT' (t.there M.! c) cs

buildMT :: (Ord c) => [c] -> ([c] -> a) -> MemoTrie c a
buildMT opts f = MT opts $ go []
  where
    go r = MT' (f $ reverse r) (M.fromList [(c, go $ c : r) | c <- opts])

memo :: (Ord c) => [c] -> OpenRec ([c] -> a) -> ([c] -> a)
memo opts f =
  let t = buildMT opts (f l)
      l = lookupMT t
   in l

part2 :: ([String], [String]) -> Integer
part2 (ts, ps) = sum $ map (expressibleWays $ map (\s -> (length s, s)) ts) ps
  where
    expressibleWays lts = memo (S.toList $ S.fromList $ concat ts) $ \f -> \case
      "" -> 1
      p -> sum [f y | (l, t) <- lts,let (x, y) = splitAt l p, x == t]

main :: IO ()
main = do
  xs <- parse <$> getData
  putStrLn "Part 1"
  print $ part1 xs
  putStrLn "Part 2"
  print $ part2 xs
