module Main where

import Data.Bits (Bits (xor))
import Data.List (tails)
import Data.Map.Strict qualified as M

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

part2 :: [Integer] -> Integer
part2 = maximum . M.elems . M.unionsWith (+) . map part2'
  where
    part2' = priceChgRuns . map (`mod` 10) . iterate step
    priceChgs ps = zipWith (\p1 p2 -> (p2 - p1, p2)) ps (tail ps)
    priceChgRuns' =
      map (\dps -> (map fst dps, snd $ last dps))
        . take (2000 - 3)
        . map (take 4)
        . tails
        . priceChgs
    priceChgRuns = M.fromListWith (\new old -> old) . priceChgRuns'

main :: IO ()
main = do
  xs <- parse <$> getData
  putStrLn "Part 1"
  print $ part1 xs
  putStrLn "Part 2"
  print $ part2 xs
