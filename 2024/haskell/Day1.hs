module Main where

import Data.List (sort)

getData :: IO String
getData = readFile "../data/day1"

parse :: String -> [(Int,Int)]
parse = map (\l -> case words l of [a, b] -> (read a, read b)) . lines

part1 :: [(Int,Int)] -> Int
part1 xs = let (l,r) = unzip xs
           in sum $ zipWith (\a b -> abs (a - b)) (sort l) (sort r)


main :: IO ()
main = getData >>= (print . part1 . parse)
