{-# LANGUAGE LambdaCase #-}

module Main where

getData :: IO String
getData = readFile "../data/day11"

parse :: String -> [Integer]
parse = map read . words

split :: Integer -> Maybe (Integer, Integer)
split n = go 0 1 n 0
  where
    -- l=10^?, h=10*l, l<=b<h (assuming no leading zeros),
    -- concatenation of a,b is original number i.e. a*h+b = n
    go l h a b
      | a < l = Nothing
      | a < h = Just (a, b)
      | otherwise = let (q, r) = quotRem a 10 in go h (10 * h) q (r * h + b)

step :: Integer -> [Integer]
step = \case
  0 -> [1]
  n
    | Just (l, r) <- split n -> [l, r]
    | otherwise -> [2024 * n]

part1 :: [Integer] -> Int
part1 ns = length $ iterate (concatMap step) ns !! 25

main :: IO ()
main = do
  xs <- parse <$> getData
  putStrLn "Part 1"
  print $ part1 xs
