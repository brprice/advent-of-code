{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Functor ((<&>))
import Data.List (foldl')

getData :: IO String
getData = readFile "../data/day2"

parse :: String -> [[Int]]
parse = map (map read . words) . lines

-- The Int is the tail of the list
data Safety
  = Unsafe
  | Empty
  | Singleton Int
  | SafeInc Int
  | SafeDec Int

safety' :: Safety -> Int -> Safety
safety' = \cases
  Unsafe _ -> Unsafe
  Empty y -> Singleton y
  (Singleton x) y | x < y && x + 3 >= y -> SafeInc y
  (Singleton x) y | x > y && x - 3 <= y -> SafeDec y
  (SafeInc x) y | x < y && x + 3 >= y -> SafeInc y
  (SafeDec x) y | x > y && x - 3 <= y -> SafeDec y
  _ _ -> Unsafe

safety :: [Int] -> Safety
safety = foldl' safety' Empty

isSafe :: [Int] -> Bool
isSafe =
  safety <&> \case
    Unsafe -> False
    _ -> True

part1 :: [[Int]] -> Int
part1 = length . filter isSafe

main :: IO ()
main = do
  xs <- parse <$> getData
  putStrLn "Part 1"
  print $ part1 xs
