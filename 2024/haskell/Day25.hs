{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Data.Either (partitionEithers)
import Data.List (transpose)

getData :: IO String
getData = readFile "../data/day25"

type Key = [Int]

type Lock = [Int]

parse :: String -> ([Key], [Lock])
parse = partitionEithers . go . ("" :) . lines
  where
    go = \case
      [] -> []
      (break (== "") . tail -> (kl, kls)) -> parseKL kl : go kls
    parseKL = \case
      (('#' : _) : xs) -> Right $ parseCols xs
      xs -> Left $ parseCols $ init xs
    parseCols = map (length . filter (== '#')) . transpose

part1 :: ([Key], [Lock]) -> Int
part1 (ks, ls) = length [() | k <- ks, l <- ls, compat k l]
  where
    compat k l = all (<= 5) $ zipWith (+) k l

main :: IO ()
main = do
  xs <- parse <$> getData
  putStrLn "Part 1"
  print $ part1 xs

-- NB: there was only one part today!
