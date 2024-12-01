module Main where

import Data.List (foldl', sort)
import Data.Map.Strict qualified as M

getData :: IO String
getData = readFile "../data/day1"

parse :: String -> [(Int,Int)]
parse = map (\l -> case words l of [a, b] -> (read a, read b)) . lines

part1 :: [(Int,Int)] -> Int
part1 xs = let (l,r) = unzip xs
           in sum $ zipWith (\a b -> abs (a - b)) (sort l) (sort r)

count :: Ord a => [a] -> M.Map a Int
count = foldl' (\m x -> M.insertWith (+) x 1 m) mempty

part2 :: [(Int,Int)] -> Int
part2 xs = let (l,r) = unzip xs
               lCount = count l
               rCount = count r
               simScores = M.mapWithKey (\k c -> k * M.findWithDefault 0 k rCount) lCount
           in sum $ M.elems simScores

main :: IO ()
main = do
    xs <- parse <$> getData
    putStrLn "Part 1"
    print $ part1 xs
    putStrLn "Part 2"
    print $ part2 xs
