module Main where

import Data.Bits (Bits (xor))

getData :: IO String
getData = readFile "../data/day22"

parse :: String -> [Integer]
parse = map read . lines

step :: Integer -> Integer
step = step' (* 2048) . step' (`div` 32) . step' (* 64)
  where
    step' f s = prune . mix s $ f s
    mix = xor
    prune = (`mod` 16777216)

part1 :: [Integer] -> Integer
part1 = sum . map ((!! 2000) . iterate step)

main :: IO ()
main = do
  xs <- parse <$> getData
  putStrLn "Part 1"
  print $ part1 xs
